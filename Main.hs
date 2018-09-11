{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
import           Control.Applicative
import           Control.Arrow                       ((&&&))
import           Control.Concurrent.ParallelIO.Local
import qualified Control.Foldl                       as F
import           Control.Monad.State.Strict
import           Data.List                           (foldl')
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Time
import           Filesystem.Path.CurrentOS           (replaceExtension)
import           Prelude                             hiding (FilePath)
import           System.IO.Temp                      (withSystemTempDirectory)
import           System.ProgressBar.State
import qualified Text.Printf                         as P
import           Turtle                              (ExitCode (..), FilePath,
                                                      between, choice, contains,
                                                      cp, d, date, decodeString,
                                                      directory, du,
                                                      encodeString, extension,
                                                      filename, fp, lstree,
                                                      match, mktree, mv, optInt,
                                                      optPath, optText, option,
                                                      options, printf, proc, rm,
                                                      suffix, switch, sz,
                                                      testdir, testfile, (%),
                                                      (<.>), (</>))
import qualified Turtle

data Opts = Opts
  { input   :: !FilePath
  , output  :: !FilePath
  , bitrate :: !Text
  , jobs    :: !Int
  , ignores :: !(Set FilePath)
  , dryRun  :: !Bool
  }

data ConvertProgress = ConvertProgress
  { start        :: {-# UNPACK #-} !UTCTime
  , now          :: {-# UNPACK #-} !UTCTime
  , convprogress :: {-# UNPACK #-} !Progress
  }

instance HasProgress ConvertProgress where
  getProgress = convprogress

f2t :: FilePath -> Text
f2t = either id id . Turtle.toText

data Mapping
  = Copy { from, to :: !FilePath }
  | Convert { from, to :: !FilePath }
  deriving (Show, Eq, Ord)

data Splitter = Splitter
  { cue   :: !Mapping
  , flac  :: !Mapping
  , toDir :: !FilePath
  } deriving (Show, Eq, Ord)

data Action
  = Map !Mapping
  | Split !Splitter
  deriving (Show, Eq, Ord)

data FsAction
  = FsCopy { fsfrom, fsto :: !FilePath }
  | FsMove { fsfrom, fsto :: !FilePath }
  deriving (Show, Eq, Ord)

mkTestExt :: Maybe Text -> Set.Set Text -> a -> a -> a
mkTestExt thing exts y n = case thing of
  Just ext | Set.member (T.toLower ext) exts -> y
  _                                          -> n

decideMappings :: Opts -> FilePath -> [Mapping]
decideMappings Opts{input, output, ignores} from
  = respectIgnores
  . ignoreDot (encodeString from)
  . ignoreDot (encodeString (filename from))
  . ignorePlaylists
  . convert320mp3dir
  . copyMusic
  . convertMusic
  . copyImages
  $ []
  where
    respectIgnores =
      if Set.member from ignores
      then const []
      else id

    testExt exts r = mkTestExt ext exts (const r) id
    ext = Turtle.extension from
    dir = T.toLower (f2t (Turtle.directory from))
    to = case Turtle.stripPrefix input from of
      Just relpath -> output </> mapBadChars relpath
      Nothing      -> error $ "could not extract prefix from " ++ show from
    ignoreDot ('.':_) = const []
    ignoreDot _       = id
    ignorePlaylists = testExt playlistExts []
    bitrate320dir =
      contains (choice
                [ between "(" ")" bitrate320
                , between "[" "]" bitrate320
                , "@" >> bitrate320
                ]) <|>
      suffix bitrate320
    bitrate320 = do
      _ <- "320"
      option $ choice
        [ "kbps"
        , "kps"
        , "kbs"
        , "k"
        ]
    conversion = Convert{from, to=replaceExtension to "ogg"}
    copy = Copy{from, to}
    convert320mp3dir =
      if ext == Just "mp3" && not (null (match bitrate320dir dir))
      then const [conversion]
      else id
    convertMusic = testExt musicExts [conversion]
    copyMusic = testExt keepMusicExts [copy]
    copyImages = testExt imageExts [copy]

splitCues :: Set Mapping -> Set Action
splitCues mappings =
  Set.map Map (mappings Set.\\ foundPairs) `Set.union`
  Set.fromList (map Split splits)
  where

    flacsAndCueMaps :: Map FilePath Mapping
    flacsAndCueMaps = Map.fromList $ do
      m        <- Set.toList mappings
      Just ext <- [extension (from m)]
      guard (ext == "cue" || ext == "flac")
      return (from m, m)

    isFlacMapping f = extension (from f) == Just "flac"

    flacMaps :: Map FilePath Mapping
    cueMaps :: Map FilePath Mapping
    (flacMaps, cueMaps) = Map.partition isFlacMapping flacsAndCueMaps

    splits =
      [ Splitter
        { cue
        , flac
        , toDir = directory (to flac)
        }
      | cue <- Map.elems cueMaps
      , flac <- case Map.lookup (replaceExtension (from cue) "flac") flacMaps of
          Just r -> [r]
          _      -> []
      ]

    foundPairs = Set.fromList (concatMap (\s -> [cue s, flac s]) splits)

interpret :: FilePath -> Opts -> Action -> IO [FsAction]
interpret _ _ (Map Copy{from, to}) =
  return [FsCopy{fsfrom=from, fsto=to}]
interpret tout opts (Map Convert{from, to}) = proc
  "ffmpeg"
  [ "-loglevel", "quiet"
  , "-i" , f2t from
  , "-vn"
  , "-codec:a", "libopus"
  , "-b:a", bitrate opts
  , "-vbr", "on"
  , "-compression_level", "10"
  , "-map_metadata", "0:g"
  , "-n"
  , f2t tout
  ] mempty >>= \case

  ExitSuccess   ->
    return [FsMove{fsfrom=tout, fsto=to}]

  ExitFailure _ ->
    return [FsCopy{fsfrom=tout, fsto=to}]

interpret tmpf opts (Split Splitter{cue, flac, toDir}) = do
  mktree tmpf
  -- Even if the cue file is left "dangling" at the end of this, we still
  -- include it in the output, just so that the "evidence" of the splitting
  -- remains, so shrinkmusic doesn't try to duplicate work on a second
  -- invocation
  --
  -- Since media players might be confused by a cue file referring to a flac
  -- that doesn't exist, we rename it to %f.cue.split, if and only if we succeed
  -- in doing the cue split.
  proc "shnsplit"
    [ "-f", f2t (from cue)
    , "-o", "flac", f2t (from flac)
    , "-t", "%n %t"
    , "-d", f2t tmpf
    ] mempty
    >>= \case

    ExitSuccess -> do
      flacOuts <- lstree tmpf `Turtle.fold` F.list >>= filterM testfile
      cueOk    <- proc "cuetag.sh" (f2t (from cue):map f2t flacOuts) mempty
      case cueOk of
        ExitSuccess   -> return ()
        ExitFailure _ -> printf ("Warning: cuetag.sh failed for " % fp % "\n") (from cue)

      (FsCopy{fsfrom=from cue, fsto=to cue} :) . concat <$> sequence
        [ interpret tout opts (Map Convert
          { from=flacOut
          , to=toDir </> replaceExtension (filename flacOut) "ogg"
          })
        | (i, flacOut) <- zip [0 :: Int ..] flacOuts
        , let tout = tmpf </> decodeString (show i)
        ]

    ExitFailure _ ->
      -- flac is probably reasonably compressable when it's a giant file, less
      -- gain to be had by converting one big flac rather than lots of littler
      -- ones
      return [ FsCopy{fsfrom=from cue, fsto=to cue}
             , FsCopy{fsfrom=from flac, fsto=to flac}
             ]

interpretFs :: FsAction -> IO ()
interpretFs FsCopy{fsfrom, fsto} = cp fsfrom fsto
interpretFs FsMove{fsfrom, fsto} = mv fsfrom fsto

mapBadChars :: FilePath -> FilePath
mapBadChars orig =
  if null onlyBads
  then orig
  else decodeString (concatMap unbad str)
  where
    bads = "?<>\\|" :: String
    unbad '|' = ")"
    unbad c   = if c `elem` bads then [] else [c]
    str = encodeString orig
    onlyBads = filter (`elem` bads) str

data PlanPhase
  = ReadingFiles
  | FilteringExisting
  | FilteringNonMusicalDirectories
  | CreatingActions
  | Done
  deriving (Show, Eq, Ord, Enum, Bounded)

main :: IO ()
main = do
  opts0@Opts{input, output, jobs, dryRun, ignores} <- options "shrinkmusic" $ Opts
    <$> fmap (</> "") (optPath "input" 'i' "Input directory")
    <*> fmap (</> "") (optPath "output" 'o' "Output directory")
    <*> optText "bitrate" 'b' "bitrate for audio"
    <*> optInt "jobs" 'j' "Number of jobs to use"
    <*> (Set.fromList `fmap` many (optPath "ignore" 'z' "Ignore this file") <|>
         pure Set.empty)
    <*> switch "dry-run" 'd' "Do nothing; just output the plan"

  ignoreRecur <- fmap (Set.fromList . concat) . forM (Set.toList ignores) $
    \i -> do
      isDir <- testdir i
      if isDir
        then lstree i `Turtle.fold` F.list >>= filterM testfile
        else return [i]

  let opts = opts0{ignores=ignoreRecur}

  -- takes *all* files so we can copy everything over

  putStrLn "constructing plan..."

  let
    planProgress :: PlanPhase -> IO ()
    planProgress pp =
      (autoProgressBar :: ProgressBar Progress (IO ()))
      exact
      noLabel
      40
      Progress
      { progressDone = toInteger (fromEnum (pp :: PlanPhase))
      , progressTodo = toInteger (fromEnum (maxBound :: PlanPhase))
      }

  planProgress ReadingFiles
  inFiles <- lstree input `Turtle.fold` F.list >>= filterM testfile

  let mappings = concatMap (decideMappings opts) inFiles
  let targets = Set.fromList
                (concatMap
                 (\s -> to s:[to s <.> "split" | extension (to s) == Just "cue" ])
                 mappings)

  -- find where files were deleted from original folder i.e., the files in the
  -- destination folder that are not accounted for by the action plan
  planProgress FilteringExisting
  !deletes <-
    (\destfiles -> Set.fromList destfiles `Set.difference` targets)
    <$> (lstree output `Turtle.fold` F.list >>= filterM testfile)

  -- find directories that have no music in them (e.g., they're cover image
  -- scans; stuff i don't care about for my phone)
  planProgress FilteringNonMusicalDirectories

  let dirs2targets =
        Map.fromListWith Set.union
        (map ((directory &&& Set.singleton) . to) mappings)
  let isMusicOrSubdir p =
        mkTestExt (extension p) musicExts True False ||
        directory p == p
  let !nonmusicdirs =
        Map.filter (not . any isMusicOrSubdir) dirs2targets

  planProgress CreatingActions

  actions <- Set.toList . splitCues . Set.fromList <$> filterM
    (\p -> do
        -- filter out where the destination file already exists
        alreadyExists <- testfile (to p)
        return (not alreadyExists && directory (to p) `Map.notMember` nonmusicdirs))
    mappings

  planProgress Done

  start0 <- date
  if dryRun
    then do
      forM_ actions $ \case
        Map Copy{to}        -> printf ("copy " % fp % "\n") to
        Map Convert{to}     -> printf ("convert " % fp % "\n") to
        Split Splitter{cue} -> printf ("split " % fp % "\n") (from cue)

      let copies = filter (\case {Map Copy{} -> True; _ -> False}) actions
      let converts = filter (\case {Map Convert{} -> True; _ -> False}) actions
      let splits = filter (\case {Split{} -> True; _ -> False}) actions
      let sizeOf f = foldl' (+) 0 <$> mapM du
            (do a <- f
                case a of
                  Map m                     -> [from m]
                  Split Splitter{flac, cue} -> [from flac, from cue])

      totalInputFileSize <- sizeOf actions
      totalCopySize <- sizeOf copies
      totalConvertSize <- sizeOf converts
      totalSplitSize <- sizeOf splits

      printf "\nSUMMARY\n"
      printf ("Actions:   \t" % d % "\t" % sz % "\n") (length actions) totalInputFileSize
      printf ("Converts:  \t" % d % "\t" % sz % "\n") (length converts) totalCopySize
      printf ("Copies:    \t" % d % "\t" % sz % "\n") (length copies) totalConvertSize
      printf ("Cue splits:\t" % d % "\t" % sz % "\n") (length splits) totalSplitSize

    else do
      putStrLn "making directory tree..."
      mapM_
        (\case
            Map m -> mktree (directory (to m))
            Split s -> mktree (toDir s))
        actions
      unless (Set.null deletes) $ do
        putStrLn "running deletes..."
        mapM_ rm deletes
      putStrLn "converting..."
      (progRef, _) <- startProgress
        exact
        (\ConvertProgress{ convprogress=Progress{progressDone, progressTodo}, start
                         , now } ->
          let
            elapsed, avg, remain :: Double
            elapsed = realToFrac (diffUTCTime now start)
            avg = elapsed / fromInteger progressDone
            remain = avg * fromInteger (progressTodo - progressDone)
          in P.printf "%.2fs elapsed. %0.2fs remaining" elapsed remain)
        40
        ConvertProgress
        { start = start0
        , now = start0
        , convprogress = Progress
          { progressDone = 0
          , progressTodo = toInteger (length actions)
          }
        }
      let
        step :: FilePath -> Action -> IO [FsAction]
        step tmpf action = do
          fs <- interpret tmpf opts action
          now <- date
          incProgress progRef $ \c -> c
            { now
            , convprogress = (convprogress c)
              { progressDone = progressDone (convprogress c) + 1
              }
            }
          return fs

      withSystemTempDirectory "shrinkmusic" $ \tempOutDir -> do
        let ftempOutDir = decodeString tempOutDir
        fsActs <- fmap concat . withPool jobs $ \pool -> parallelInterleaved pool
          [ step tmpf act
          | (i, act) <- zip [0 :: Int ..] actions
          , let tmpf = ftempOutDir </> decodeString (show i)
          ]
        print (length fsActs)
        mapM_ (\fs -> do
                  putChar '\n'
                  print fs
                  putChar '\n'
                  interpretFs fs) fsActs

playlistExts :: Set.Set Text
playlistExts = Set.fromList
  ["m3u", "pls", "asx", "xspf", "wpl"]

musicExts :: Set.Set Text
musicExts = Set.fromList
  [ "amr", "mp1", "mp2", "mp3", "spx", "gsm", "wma", "aac", "mpc", "vqf"
  , "ots", "vox", "voc", "dwd", "smp", "ogg", "ra", "rm", "flac", "la"
  , "pac", "ape", "ofr", "ofs", "off", "rka", "shn", "tak", "tta", "wv"
  , "wma", "brstm", "dts", "dtshd", "dtsma", "ast", "aw", "psf", "webm", "wav"
  , "m4a" ]

keepMusicExts :: Set.Set Text
keepMusicExts = Set.fromList
  [ "ogg", "mp3", "opus", "aac", "mp2", "mp1", "cue" ]

imageExts :: Set.Set Text
imageExts = Set.fromList
  [ "ase", "art", "bmp", "blp", "cd5", "cit", "cpt", "cr2", "cut"
  , "dds", "dib", "djvu", "egt", "exif", "gif", "gpl", "grf", "icns"
  , "ico", "iff", "jng", "jpeg", "jpg", "jfif", "jp2", "jps", "lbm"
  , "max", "miff", "mng", "msp", "nitf", "ota", "pbm", "pc1", "pc2"
  , "pc3", "pcf", "pcx", "pdn", "pgm", "pi1", "pi2", "pi3", "pict", "pct"
  , "pnm", "pns", "ppm", "psb", "psd", "pdd", "psp", "px", "pxm", "pxr"
  , "qfx", "raw", "rle", "sct", "sgi", "rgb", "int", "bw", "tga"
  , "tiff", "tif", "vtf", "xbm", "xcf", "xpm", "3dv", "amf", "ai", "awg"
  , "cgm", "cdr", "cmx", "dxf", "e2d", "egt", "eps", "fs", "gbr", "odg"
  , "svg", "stl", "vrml", "x3d", "sxd", "v2d", "vnd", "wmf", "emf"
  , "art", "xar", "png", "webp", "jxr", "hdp", "wdp", "cur", "ecw", "iff"
  , "lbm", "liff", "nrrd", "pam", "pcx", "pgf", "sgi", "rgb", "rgba"
  , "bw", "int", "inta", "sid", "ras", "sun", "tga" ]

