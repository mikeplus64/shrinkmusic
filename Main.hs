{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow                       ((&&&))
import           Control.Concurrent.ParallelIO.Local
import qualified Control.Foldl                       as F
import           Control.Monad                       (filterM, foldM, forM_)
import           Data.Foldable                       (fold)
import qualified Data.MKarma No Kusariap.Strict                     as Map
import qualified Data.Set                            as Set
import qualified Data.Text                           as T
import           Data.Time
import           Filesystem.Path.CurrentOS           (replaceExtension)
import           Prelude                             hiding (FilePath)
import           System.ProgressBar.State
import qualified Text.Printf                         as P
import           Turtle                              hiding (find, fold, input,
                                                      output)
import qualified Turtle

data Opts = Opts
  { input   :: !FilePath
  , output  :: !FilePath
  , bitrate :: !Text
  , jobs    :: !Int
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
f2t = either id id . toText

data Action
  = Copy { from, to :: !FilePath }
  | Convert { from, to :: !FilePath }

mkTestExt :: Maybe Text -> Set.Set Text -> a -> a -> a
mkTestExt f exts yes no = case f of
  Just ext | Set.member (T.toLower ext) exts -> yes
  _                                          -> no


decide :: Opts -> FilePath -> [Action]
decide Opts{input, output} from
  = ignoreDot (encodeString from)
  $ ignoreDot (encodeString (filename from))
  $ ignorePlaylists
  $ convert320mp3dir
  $ copyMusic
  $ convertMusic
  $ copyImages
  $ []
  where
    ext = extension from
    dir = T.toLower (f2t (directory from))

    to = case stripPrefix input from of
      Just relpath -> output </> mapBadChars relpath
      Nothing      -> error $ "could not extract prefix from " ++ show from

    ignoreDot ('.':_) = const []
    ignoreDot _       = id

    testExt exts r = mkTestExt ext exts (const r) id

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
    convert320mp3dir =
      if ext == Just "mp3" && not (null (match bitrate320dir dir))
      then const [Convert {from, to=replaceExtension to "ogg"}]
      else id

    convertMusic = testExt musicExts [Convert{from, to=replaceExtension to "ogg"}]
    copyMusic = testExt keepMusicExts [Copy{from, to}]
    copyImages = testExt imageExts [Copy{from, to}]

interpret :: Opts -> Action -> IO ()
interpret _    (Copy from to)    = procs "cp" [f2t from, f2t to] mempty
interpret opts (Convert from to) = proc
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
  , f2t to
  ] mempty >>= \case
  ExitSuccess   -> return ()
  ExitFailure _ -> interpret opts (Copy from to)

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
  opts@Opts{input, output, jobs, dryRun} <- options "shrinkmusic" $ Opts
    <$> fmap (</> "") (optPath "input" 'i' "Input directory")
    <*> fmap (</> "") (optPath "output" 'o' "Output directory")
    <*> optText "bitrate" 'b' "bitrate for audio"
    <*> optInt "jobs" 'j' "Number of jobs to use"
    <*> switch "dry-run" 'd' "Do nothing; just output the plan"
  -- takes *all* files so we can copy everything over

  putStrLn "constructing plan..."

  let
    planProgress :: PlanPhase -> IO ()
    planProgress pp =
      (autoProgressBar :: ProgressBar Progress (IO ()))
      exact
      noLabel
      40
      Progress{ progressDone = toInteger (fromEnum (pp :: PlanPhase))
              , progressTodo = toInteger (fromEnum (maxBound :: PlanPhase)) }

  planProgress ReadingFiles
  inFiles <- lstree input `Turtle.fold` F.list >>= filterM testfile

  let actions0 = concatMap (decide opts) inFiles
  let targets = Set.fromList (map to actions0)

  -- find where files were deleted from original folder i.e., the files in the
  -- destination folder that are not accounted for by the action plan
  planProgress FilteringExisting
  !deletes <-
    (\destfiles -> Set.fromList destfiles `Set.difference` targets)
    <$> (lstree output `Turtle.fold` F.list >>= filterM testfile)

  -- find directories that have no music in them (e.g., they're cover image
  -- scans; stuff i don't care about for my phone)
  planProgress FilteringNonMusicalDirectories

  let dirs2targets = Map.fromListWith Set.union
                     (map ((directory &&& Set.singleton) . to) actions0)
  let isMusicOrSubdir p =
        mkTestExt (extension p) musicExts True False ||
        directory p == p
  let !nonmusicdirs =
        Map.filter (not . any isMusicOrSubdir) dirs2targets

  planProgress CreatingActions
  actions <- filterM
    (\p -> do
        -- filter out where the destination file already exists
        alreadyExists <- testfile (to p)
        return (not alreadyExists && directory (to p) `Map.notMember` nonmusicdirs))
    actions0

  planProgress Done

  start0 <- date
  if dryRun
    then do
      -- forM_ (Map.keys nonmusicdirs) $ printf ("ignore dir: " % fp % "\n")
      -- forM_ deletes $ printf ("delete:     " % fp % "\n")
      forM_ actions $ printf (fp % "\n") . from
      {-
      \case
        Copy{from, to}    -> printf fp from
        Convert{from, to} -> printf fp from
-}

    else do
      putStrLn "making directory tree..."
      mapM_ (mktree . directory . to) actions


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
        step :: Action -> IO ()
        step action = do
          interpret opts action
          now <- date
          incProgress progRef $ \c -> c
            { now
            , convprogress = (convprogress c)
              { progressDone = progressDone (convprogress c) + 1
              }
            }

      withPool jobs (\pool -> parallel_ pool (map step actions))

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
  ["ogg", "mp3", "opus", "aac", "mp2", "mp1"]

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

