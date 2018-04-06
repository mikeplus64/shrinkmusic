{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent.ParallelIO.Local
import qualified Control.Foldl                       as F
import           Control.Monad                       (filterM)
import           Data.Maybe                          (fromJust)
import qualified Data.Set                            as Set
import qualified Data.Text                           as T
import           Data.Time
import           Filesystem.Path.CurrentOS           (replaceExtension)
import           Prelude                             hiding (FilePath)
import           System.ProgressBar.State
import qualified Text.Printf                         as P
import           Turtle                              hiding (input, output)

data Opts = Opts
  { input   :: !FilePath
  , output  :: !FilePath
  , bitrate :: !Text
  , jobs    :: !Int
  }

data Conversion
  = Ignored !FilePath
  | Copied !FilePath !FilePath
  | Converted !FilePath !FilePath
  deriving (Show)

data ConvertProgress = ConvertProgress
  { start        :: {-# UNPACK #-} !UTCTime
  , now          :: {-# UNPACK #-} !UTCTime
  , convprogress :: {-# UNPACK #-} !Progress
  }

instance HasProgress ConvertProgress where
  getProgress = convprogress

f2t :: FilePath -> Text
f2t = either id id . toText

inc :: ProgressRef ConvertProgress -> Action -> (FilePath, FilePath) -> IO ()
inc ref _dec _frto = do
  now <- date
  incProgress ref $ \c -> c
    { now
    , convprogress = (convprogress c)
      { progressDone = progressDone (convprogress c) + 1 }
    }
  {-
  where
    mkConversion Copy (from, to)    = Copied from to
    mkConversion Ignore (from, _)   = Ignored from
    mkConversion Convert (from, to) = Converted from to -}

data Action
  = Copy
  | Convert
  | Ignore

decide :: FilePath -> IO Action
decide from
  = ignoreDot (encodeString from)
  $ ignoreDot (encodeString (filename from))
  $ ignorePlaylists
  $ copyMusic
  $ convertMusic
  $ copyImages
  $ return Ignore
  where
    ignoreDot ('.':_) = const (return Ignore)
    ignoreDot _       = id
    testExt exts r = case extension from of
      Just ext | Set.member (T.toLower ext) exts -> const (return r)
      _        -> id
    ignorePlaylists = testExt playlistExts Ignore
    convertMusic = testExt musicExts Convert
    copyMusic = testExt keepMusicExts Copy
    copyImages = testExt imageExts Copy

convertOrCopy :: Opts -> (FilePath, FilePath) -> IO Action
convertOrCopy Opts{bitrate} (from, to) = decide from >>= \case
  Ignore  -> return Ignore
  Copy    -> Copy <$ procs "cp" [f2t from, f2t to] mempty
  Convert -> do
    code <- proc
      "ffmpeg"
      [ "-loglevel", "quiet"
      , "-i" , f2t from
      , "-codec:a", "libopus"
      , "-b:a", bitrate
      , "-vbr", "on"
      , "-compression_level", "10"
      , "-map_metadata", "0:g"
      , "-n"
      , f2t (replaceExtension to "ogg")
      ]
      mempty
    case code of
      ExitSuccess   -> return Convert
      ExitFailure _ -> Copy <$ procs "cp" [f2t from, f2t to] mempty

mapBadChars :: FilePath -> FilePath
mapBadChars orig =
  if null onlyBads
  then orig
  else decodeString (concatMap unbad str)
  where
    bads = "?<>\\" :: String
    unbad c = if c `elem` bads then [] else [c]
    str = encodeString orig
    onlyBads = filter (`elem` bads) str

main :: IO ()
main = do
  opts@Opts{input, output, jobs} <- options "shrinkmusic" $ Opts
    <$> fmap (</> "") (optPath "input" 'i' "Input directory")
    <*> fmap (</> "") (optPath "output" 'o' "Output directory")
    <*> optText "bitrate" 'b' "bitrate for audio"
    <*> optInt "jobs" 'j' "Number of jobs to use"

  -- takes *all* files so we can copy everything over
  absStuff <- lstree input `fold` F.list
  absFiles <- filterM testfile absStuff

  let filesAndRels = [ (absf, fromJust (stripPrefix input absf)) | absf <- absFiles ]
  let dirs = Set.fromList (map ((output </>) . directory . snd) filesAndRels)
  let files = map (\(absf, relf) -> (absf, output </> mapBadChars relf)) filesAndRels
  mapM_ mktree dirs
  start0 <- date

  (progRef, _) <- startProgress
    exact
    (\ConvertProgress{ convprogress=Progress{progressDone, progressTodo}
                     , start
                     , now
                     } ->
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
      , progressTodo = toInteger (length files)
      }
    }
  withPool jobs $ \pool ->
    parallel_ pool (map (\q -> do dec <- convertOrCopy opts q
                                  inc progRef dec q) files)

playlistExts :: Set.Set Text
playlistExts = Set.fromList
  ["m3u", "pls", "asx", "xspf", "wpl"]

musicExts :: Set.Set Text
musicExts = Set.fromList
  [ "amr", "mp1", "mp2", "mp3", "spx", "gsm", "wma", "aac", "mpc", "vqf"
  , "ots", "vox", "voc", "dwd", "smp", "ogg", "ra", "rm", "flac", "la"
  , "pac", "ape", "ofr", "ofs", "off", "rka", "shn", "tak", "tta", "wv"
  , "wma", "brstm", "dts", "dtshd", "dtsma", "ast", "aw", "psf"]

keepMusicExts :: Set.Set Text
keepMusicExts = Set.fromList
  ["ogg", "mp3", "opus", "aac"]

imageExts :: Set.Set Text
imageExts = Set.fromList
  [ "ase", "art", "bmp", "blp", "cd5", "cit", "cpt", "cr2", "cut"
  , "dds", "dib", "djvu", "egt", "exif", "gif", "gpl", "grf", "icns"
  , "ico", "iff", "jng", "jpeg", "jpg", "jfif", "jp2", "jps", "lbm"
  , "max", "miff", "mng", "msp", "nitf", "ota", "pbm", "pc1", "pc2"
  , "pc3", "pcf", "pcx", "pdn", "pgm", "PI1", "PI2", "PI3", "pict", "pct"
  , "pnm", "pns", "ppm", "psb", "psd", "pdd", "psp", "px", "pxm", "pxr"
  , "qfx", "raw", "rle", "sct", "sgi", "rgb", "int", "bw", "tga"
  , "tiff", "tif", "vtf", "xbm", "xcf", "xpm", "3dv", "amf", "ai", "awg"
  , "cgm", "cdr", "cmx", "dxf", "e2d", "egt", "eps", "fs", "gbr", "odg"
  , "svg", "stl", "vrml", "x3d", "sxd", "v2d", "vnd", "wmf", "emf"
  , "art", "xar", "png", "webp", "jxr", "hdp", "wdp", "cur", "ecw", "iff"
  , "lbm", "liff", "nrrd", "pam", "pcx", "pgf", "sgi", "rgb", "rgba"
  , "bw", "int", "inta", "sid", "ras", "sun", "tga" ]

