{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
import           Control.Applicative
import           Control.Arrow                  ( (&&&) )
import           Control.Concurrent.ParallelIO.Local
import qualified Control.Foldl                 as F
import           Control.Monad.State.Strict
import           Data.List                      ( foldl'
                                                , sort
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Read                as T
import           Data.Time
import           Filesystem.Path.CurrentOS      ( replaceExtension )
import           GHC.IO.Encoding                ( setLocaleEncoding
                                                , utf8
                                                )
import           Prelude                 hiding ( FilePath )
import qualified System.IO                     as IO
                                                ( hSetEncoding
                                                , stderr
                                                , stdout
                                                , utf8
                                                )
import           System.IO.Temp                 ( withSystemTempDirectory )
import           System.ProgressBar
import qualified Text.Printf                   as P
import           Turtle                         ( (%)
                                                , (</>)
                                                , ExitCode(..)
                                                , FilePath
                                                , between
                                                , choice
                                                , contains
                                                , d
                                                , date
                                                , decodeString
                                                , directory
                                                , du
                                                , encodeString
                                                , extension
                                                , filename
                                                , fp
                                                , inprocWithErr
                                                , lineToText
                                                , lstree
                                                , match
                                                , mktree
                                                , optInt
                                                , optPath
                                                , optText
                                                , option
                                                , options
                                                , printf
                                                , proc
                                                , procs
                                                , rm
                                                , suffix
                                                , switch
                                                , sz
                                                , testdir
                                                , testfile
                                                )
import qualified Turtle

data Opts = Opts
  { input :: !FilePath
  , output :: !FilePath
  , bitrate :: !Text
  , jobs :: !Int
  , ignores :: !(Set FilePath)
  , dryRun :: !Bool
  , overwriteExisting :: !Bool
  , convertAll :: !Bool
  , outputFormat :: !Text
  }

f2t :: FilePath -> Text
f2t = either id id . Turtle.toText

data Mapping
  = Copy { from, to :: !FilePath }
  | Convert { from, to :: !FilePath }
  deriving (Show, Eq, Ord)

data Splitter = Splitter
  { cue :: !Mapping
  , flac :: !Mapping
  , toDir :: !FilePath
  }
  deriving (Show, Eq, Ord)

data Action
  = Map !Mapping
  | Split !Splitter
  deriving (Show, Eq, Ord)

mkTestExt :: Maybe Text -> Set.Set Text -> a -> a -> a
mkTestExt thing exts y n = case thing of
  Just ext | Set.member (T.toLower ext) exts -> y
  _ -> n

decideMappings :: Opts -> FilePath -> IO [Mapping]
decideMappings Opts { input, output, ignores, convertAll, outputFormat } from =
  respectIgnores
    . ignoreDot (encodeString from)
    . ignoreDot (encodeString (filename from))
    . ignorePlaylists
    . (if convertAll
        then convertMusic
        else convert320mp3dir . copyMusic . convertMusic
      )
    . copyCue
    . copyImages
    $ return []
 where
  finish :: a -> b -> IO a
  finish x _ = return x

  continue :: IO a -> IO a
  continue = id

  respectIgnores = if Set.member from ignores then finish [] else continue

  testExt exts r = mkTestExt ext exts (finish r) continue
  ext = Turtle.extension from
  dir = T.toLower (f2t (Turtle.directory from))
  base = T.toLower (f2t (Turtle.basename from))
  to = case Turtle.stripPrefix input from of
    Just relpath -> output </> mapBadChars relpath
    Nothing -> error $ "could not extract prefix from " ++ show from

  ignoreDot ('.' : _) = finish []
  ignoreDot _ = continue

  ignorePlaylists = testExt playlistExts []

  bitrate320dir = contains (choice [("(" <|> "[" <|> "@") >> bitrate320])
    <|> suffix bitrate320

  bitrate320 = do
    _ <- "320"
    option $ choice ["kbps", "kps", "kbs", "k"]

  conversion =
    Convert { from, to = replaceExtension to (outputExtension outputFormat) }

  copy = Copy { from, to }

  convert320mp3dir =
    if ext == Just "mp3" && not (null (match bitrate320dir dir))
      then finish [conversion]
      else continue

  convertMusic = testExt musicExts [conversion]
  copyMusic = testExt keepMusicExts [copy]

  copyImages =
    if maybe False (`Set.member` imageExts) ext
       && Set.member base coverImageFiles
    then
      finish [copy]
    else
      continue

  copyCue = if ext == Just "cue" then finish [copy] else continue

outputExtension :: Text -> Text
outputExtension format = case format of
  "mp3" -> "mp3"
  "flac" -> "flac"
  "ogg" -> "ogg"
  "opus" -> "ogg"
  _ -> error "unknown format"

{-
bitrateOf f = do
  br <- fmap (T.decimal . lineToText . either id id) <$> inprocWithErr "ffprobe"
    [ "-v", "quiet"
    , "-select_streams", "a:0"
    , "-show_entries", "stream=bit_rate"
    , "-of", "default=noprint_wrappers=1:nokey=1"
    , f2t f
    ] mempty `Turtle.fold` F.head
  case br of
    Just (Right (bitrate, _)) -> return (bitrate :: Int)
    err                       -> error (show err)
-}

splitCues :: Set Mapping -> Set Action
splitCues mappings =
  Set.map
      Map
      (Set.filter (\m -> extension (to m) /= Just "cue")
                  (mappings Set.\\ foundPairs)
      )
    `Set.union` Set.fromList (map Split splits)
 where

  flacsAndCueMaps :: Map FilePath Mapping
  flacsAndCueMaps = Map.fromList $ do
    m <- Set.toList mappings
    Just ext <- [extension (from m)]
    guard (ext == "cue" || ext == "flac")
    return (from m, m)

  isFlacMapping f = extension (from f) == Just "flac"

  flacMaps :: Map FilePath Mapping
  cueMaps :: Map FilePath Mapping
  (flacMaps, cueMaps) = Map.partition isFlacMapping flacsAndCueMaps

  splits =
    [ Splitter { cue, flac, toDir = directory (to flac) }
    | cue <- Map.elems cueMaps
    , flac <- case Map.lookup (replaceExtension (from cue) "flac") flacMaps of
      Just r -> [r]
      _ -> []
    ]

  foundPairs = Set.fromList (concatMap (\s -> [cue s, flac s]) splits)

interpret :: Opts -> Action -> IO ()
interpret _ (Map Copy { from, to }) =
  void (proc "cp" [f2t from, f2t to] mempty)

interpret opts (Map Convert { from, to })
  | extension from == Just "flac" && extension to == Just "flac" = interpret
    opts
    (Map Copy { from, to })

interpret opts (Map Convert { from, to }) = proc "ffmpeg" args mempty >>= \case
  ExitSuccess -> return ()
  ExitFailure _ -> interpret opts (Map Copy { from, to })

 where
  args = case outputFormat opts of
    "mp3" ->
      [ "-loglevel"
      , "error"
      , "-i"
      , f2t from
      , "-vn"
      , "-codec:a"
      , "libmp3lame"
      , "-b:a"
      , bitrate opts
      , "-map_metadata"
      , "0:g"
      , "-n"
      , f2t to
      ]

    "wma" ->
      [ "-loglevel"
      , "error"
      , "-i"
      , f2t from
      , "-vn"
      , "-codec:a"
      , "wmav2"
      , "-b:a"
      , bitrate opts
      , "-vbr"
      , "on"
      , "-map_metadata"
      , "0:g"
      , "-n"
      , f2t to
      ]

    "flac" ->
      [ "-loglevel"
      , "error"
      , "-i"
      , f2t from
      , "-vn"
      , "-codec:a"
      , "flac"
      , "-map_metadata"
      , "0:g"
      , "-n"
      , f2t to
      ]

    _ -> -- default to opus
      [ "-loglevel"
      , "error"
      , "-i"
      , f2t from
      , "-vn"
      , "-codec:a"
      , "libopus"
      , "-b:a"
      , bitrate opts
      , "-vbr"
      , "on"
      , "-compression_level"
      , "10"
      , "-map_metadata"
      , "0:g"
      , "-n"
      , f2t to
      ]

interpret opts (Split Splitter { cue, flac, toDir }) =
  withSystemTempDirectory "shrinkmusic" $ \tmpl -> do
  -- Even if the cue file is left "dangling" at the end of this, we still
  -- include it in the output, just so that the "evidence" of the splitting
  -- remains, so shrinkmusic doesn't try to duplicate work on a second
  -- invocation
  --
  -- Since media players might be confused by a cue file referring to a flac
  -- that doesn't exist, we rename it to %f.cue.split, if and only if we succeed
  -- in doing the cue split.
    let !tmpf = decodeString tmpl
    proc
        "shnsplit"
        [ "-f"
        , f2t (from cue)
        , "-o"
        , "flac"
        , f2t (from flac)
        , "-t"
        , "%n %t"
        , "-d"
        , T.pack tmpl
        , "-q"
        ]
        mempty
      >>= \case

            ExitSuccess -> do
              flacOuts <- lstree tmpf `Turtle.fold` F.list >>= filterM testfile --
              cueOk <- proc "cuetag.sh"
                            (f2t (from cue) : sort (map f2t flacOuts))
                            mempty
              case cueOk of
                ExitSuccess -> return ()
                ExitFailure _ -> printf
                  ("Warning: cuetag.sh failed for " % fp % "\n")
                  (from cue)

              interpret opts (Map cue { to = to cue })

              mapM_
                (interpret opts)
                [ Map Convert
                    { from = out
                    , to = toDir </> replaceExtension
                             (mapBadChars (filename out))
                             (outputExtension (outputFormat opts))
                    }
                | out <- flacOuts
                ]

            ExitFailure _ -> do
              -- flac is probably reasonably compressable when it's a giant file, less
              -- gain to be had by converting one big flac rather than lots of littler
              -- ones
              interpret opts (Map cue)
              interpret opts (Map flac)

mapBadChars :: FilePath -> FilePath
mapBadChars orig = if null onlyBads
  then orig
  else decodeString (concatMap unbad str)
 where
  bads = "\"'?<>\\|" :: String
  unbad '|' = ")"
  unbad c = if c `elem` bads then [] else [c]
  str = encodeString orig
  onlyBads = filter (`elem` bads) str

data PlanPhase
  = ReadingFiles
  | FindingDeleted
  | FilteringNonMusicalDirectories
  | CreatingActions
  | Done
  deriving (Show, Eq, Ord, Enum, Bounded)

data ConvertProgress = ConvertProgress
  { start :: {-# UNPACK #-} !UTCTime
  , now :: {-# UNPACK #-} !UTCTime
  }

main :: IO ()
main = do
  setLocaleEncoding utf8
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8

  opts0@Opts { input, output, jobs, dryRun, ignores, overwriteExisting } <-
    options "shrinkmusic"
    $ Opts
    <$> fmap (</> "") (optPath "input" 'i' "Input directory")
    <*> fmap (</> "") (optPath "output" 'o' "Output directory")
    <*> optText "bitrate" 'b' "bitrate for audio"
    <*> optInt "jobs" 'j' "Number of jobs to use"
    <*> (Set.fromList
        `fmap` many (optPath "ignore" 'z' "Ignore this file")
        <|> pure Set.empty
        )
    <*> switch "dry-run" 'd' "Do nothing; just output the plan"
    <*> switch "overwrite-existing"
               'X'
               "Overwrite existing files in the destination."
    <*> switch "convert-all" 'C' "convert every single file"
    <*> optText "output-format" 'f' "format for the outputs"

  ignoreRecur <-
    fmap (Set.fromList . concat) . forM (Set.toList ignores) $ \i -> do
      isDir <- testdir i
      if isDir
        then do
          yall <- lstree i `Turtle.fold` F.list
          filterM testfile yall
        else return [i]

  let opts = opts0 { ignores = ignoreRecur }

  -- takes *all* files so we can copy everything over

  putStrLn "constructing plan..."

  planPP <- newProgressBar
    defStyle
      { stylePrefix = Label \Progress { progressCustom } _ ->
                        TL.pack (show progressCustom)
      }
    15
    Progress { progressDone = fromIntegral (fromEnum (minBound :: PlanPhase))
             , progressTodo = fromIntegral (fromEnum (maxBound :: PlanPhase))
             , progressCustom = minBound :: PlanPhase
             }

  let
    planProgress :: PlanPhase -> IO ()
    planProgress pp = updateProgress
      planPP
      \prog ->
        prog { progressDone = fromIntegral (fromEnum pp), progressCustom = pp }

  planProgress ReadingFiles
  inFiles <- lstree input `Turtle.fold` F.list >>= filterM testfile
  mappings <- concat <$> mapM (decideMappings opts) inFiles
  let targets = Set.fromList (map to mappings)

  -- find where files were deleted from original folder i.e., the files in the
  -- destination folder that are not accounted for by the action plan
  planProgress FindingDeleted

  -- pre-existing files in the output directory
  destfiles <-
    Set.fromList <$> (lstree output `Turtle.fold` F.list >>= filterM testfile)
  let !deletes =
        -- what _is_ in destination that is not in targets
        destfiles `Set.difference` targets

  -- find directories that have no music in them (e.g., they're cover image
  -- scans; stuff i don't care about for my phone)
  planProgress FilteringNonMusicalDirectories

  let dirs2targets = Map.fromListWith
        Set.union
        (map ((directory &&& Set.singleton) . to) mappings)
  let isMusicOrSubdir p =
        mkTestExt (extension p) musicExts True False || directory p == p
  let !nonmusicdirs = Map.filter (not . any isMusicOrSubdir) dirs2targets

  planProgress CreatingActions

  actions <- Set.toList . splitCues . Set.fromList <$> withPool
    jobs
    \pool -> parallelFilterM
      pool
      (\p -> if overwriteExisting
        then pure True
        else not <$> or_
          [
            -- it already exists, and they're the same file (for copy actions)
            and_
            [ testfile (to p) -- dest file already exists...

              -- and...
              -- - file sizes are the same (used as proxy for file equality)
              --   if it's a copy action; or
              -- - the file just already exists for a convert action
            , or_
              [ case p of
                Copy { from, to } -> do
                  fs <- du from
                  ts <- du to
                  when
                    (fs /= ts)
                    (printf ("mismatch " % fp % " from " % fp % "\n") to from)
                  pure (fs == ts)
                _ -> pure False
              , case p of
                Convert { from, to } -> pure True
                _ -> pure False
              ]
            ]
                 -- or, the directory is not a music directory
          , pure (directory (to p) `Map.member` nonmusicdirs)
          ]
      )
      mappings

  planProgress Done

  start0 <- date
  if dryRun
    then do
      forM_ actions $ \case
        Map Copy { to } -> printf ("copy " % fp % "\n") to
        Map Convert { to } -> printf ("convert " % fp % "\n") to
        Split Splitter { cue } -> printf ("split " % fp % "\n") (from cue)

      let copies = filter
            (\case
              Map Copy{} -> True
              _ -> False
            )
            actions
      let converts = filter
            (\case
              Map Convert{} -> True
              _ -> False
            )
            actions
      let splits = filter
            (\case
              Split{} -> True
              _ -> False
            )
            actions
      let sizeOf f = foldl' (+) 0 <$> mapM
            du
            (do
              a <- f
              case a of
                Map m -> [from m]
                Split Splitter { flac, cue } -> [from flac, from cue]
            )

      totalInputFileSize <- sizeOf actions
      totalCopySize <- sizeOf copies
      totalConvertSize <- sizeOf converts
      totalSplitSize <- sizeOf splits

      printf "\nSUMMARY\n"
      printf ("Actions:   \t" % d % "\t" % sz % "\n")
             (length actions)
             totalInputFileSize
      printf ("Converts:  \t" % d % "\t" % sz % "\n")
             (length converts)
             totalConvertSize
      printf ("Copies:    \t" % d % "\t" % sz % "\n")
             (length copies)
             totalCopySize
      printf ("Cue splits:\t" % d % "\t" % sz % "\n")
             (length splits)
             totalSplitSize
    else do
      putStrLn "making directory tree..."
      mapM_
        (\case
          Map m -> mktree (directory (to m))
          Split s -> mktree (toDir s)
        )
        actions
      unless (Set.null deletes) $ do
        putStrLn "running deletes..."
        mapM_ rm deletes
      putStrLn "converting..."
      convertPP <- newProgressBar
        defStyle
          { stylePostfix = Label
            \Progress { progressDone, progressTodo, progressCustom = ConvertProgress { start, now } } _ ->
              let
                elapsed, avg, remain :: Double
                elapsed = realToFrac (diffUTCTime now start)
                avg = elapsed / fromIntegral progressDone
                remain = avg * fromIntegral (progressTodo - progressDone)
              in
                TL.pack
                  (P.printf "%.2fs elapsed. %0.2fs remaining" elapsed remain)
          }
        15
        Progress
          { progressCustom = ConvertProgress { start = start0, now = start0 }
          , progressDone = 0
          , progressTodo = fromIntegral (length actions)
          }

      let step :: Action -> IO ()
          step action = do
            interpret opts action
            now <- date
            updateProgress
              convertPP
              \c -> c { progressCustom = (progressCustom c) { now }
                      , progressDone = progressDone c + 1
                      }

      withPool jobs \pool -> parallel_ pool (map step actions)

playlistExts :: Set.Set Text
playlistExts = Set.fromList ["m3u", "pls", "asx", "xspf", "wpl"]

musicExts :: Set.Set Text
musicExts = Set.fromList
  [ "amr"
  , "mp1"
  , "mp2"
  , "mp3"
  , "spx"
  , "gsm"
  , "wma"
  , "aac"
  , "mpc"
  , "vqf"
  , "ots"
  , "vox"
  , "voc"
  , "dwd"
  , "smp"
  , "ogg"
  , "ra"
  , "rm"
  , "flac"
  , "la"
  , "pac"
  , "ape"
  , "ofr"
  , "ofs"
  , "off"
  , "rka"
  , "shn"
  , "tak"
  , "tta"
  , "wv"
  , "wma"
  , "brstm"
  , "dts"
  , "dtshd"
  , "dtsma"
  , "ast"
  , "aw"
  , "psf"
  , "webm"
  , "wav"
  , "m4a"
  ]

keepMusicExts :: Set.Set Text
keepMusicExts = Set.fromList ["ogg", "mp3", "opus", "aac", "mp2", "mp1", "cue"]

imageExts :: Set.Set Text
imageExts = Set.fromList
  [ "ase"
  , "art"
  , "bmp"
  , "blp"
  , "cd5"
  , "cit"
  , "cpt"
  , "cr2"
  , "cut"
  , "dds"
  , "dib"
  , "djvu"
  , "egt"
  , "exif"
  , "gif"
  , "gpl"
  , "grf"
  , "icns"
  , "ico"
  , "iff"
  , "jng"
  , "jpeg"
  , "jpg"
  , "jfif"
  , "jp2"
  , "jps"
  , "lbm"
  , "max"
  , "miff"
  , "mng"
  , "msp"
  , "nitf"
  , "ota"
  , "pbm"
  , "pc1"
  , "pc2"
  , "pc3"
  , "pcf"
  , "pcx"
  , "pdn"
  , "pgm"
  , "pi1"
  , "pi2"
  , "pi3"
  , "pict"
  , "pct"
  , "pnm"
  , "pns"
  , "ppm"
  , "psb"
  , "psd"
  , "pdd"
  , "psp"
  , "px"
  , "pxm"
  , "pxr"
  , "qfx"
  , "raw"
  , "rle"
  , "sct"
  , "sgi"
  , "rgb"
  , "int"
  , "bw"
  , "tga"
  , "tiff"
  , "tif"
  , "vtf"
  , "xbm"
  , "xcf"
  , "xpm"
  , "3dv"
  , "amf"
  , "ai"
  , "awg"
  , "cgm"
  , "cdr"
  , "cmx"
  , "dxf"
  , "e2d"
  , "egt"
  , "eps"
  , "fs"
  , "gbr"
  , "odg"
  , "svg"
  , "stl"
  , "vrml"
  , "x3d"
  , "sxd"
  , "v2d"
  , "vnd"
  , "wmf"
  , "emf"
  , "art"
  , "xar"
  , "png"
  , "webp"
  , "jxr"
  , "hdp"
  , "wdp"
  , "cur"
  , "ecw"
  , "iff"
  , "lbm"
  , "liff"
  , "nrrd"
  , "pam"
  , "pcx"
  , "pgf"
  , "sgi"
  , "rgb"
  , "rgba"
  , "bw"
  , "int"
  , "inta"
  , "sid"
  , "ras"
  , "sun"
  , "tga"
  ]

coverImageFiles :: Set.Set Text
coverImageFiles = Set.fromList ["album", "cover", "front", "artist"]

--------------------------------------------------------------------------------

or_ :: Monad m => [m Bool] -> m Bool
or_ (fx : fxs) = do
  x <- fx
  if x then pure True else or_ fxs
or_ [] = pure False

and_ :: Monad m => [m Bool] -> m Bool
and_ (fx : fxs) = do
  x <- fx
  if x then and_ fxs else pure False
and_ [] = pure True

parallelFilterM :: Pool -> (a -> IO Bool) -> [a] -> IO [a]
parallelFilterM pool test xs = map snd . filter fst <$> parallel
  pool
  [ do
      ok <- test x
      pure (ok, x)
  | x <- xs
  ]
