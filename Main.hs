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

data ConvertProgress = ConvertProgress
  { message      :: !Text
  , start        :: {-# UNPACK #-} !UTCTime
  , now          :: {-# UNPACK #-} !UTCTime
  , convprogress :: {-# UNPACK #-} !Progress
  }

instance HasProgress ConvertProgress where
  getProgress = convprogress

f2t :: FilePath -> Text
f2t = either id id . toText

inc :: ProgressRef ConvertProgress -> () -> IO ()
inc ref message = do
  now <- date
  incProgress ref $ \c -> c
    { message = "ok"
    , now
    , convprogress = (convprogress c)
      { progressDone = progressDone (convprogress c) + 1
      }
    }

shouldSkip :: FilePath -> FilePath -> IO Bool
shouldSkip from to =
  if case extension from of
    Just "m3u"  -> True
    Just "pls"  -> True
    Just "asx"  -> True
    Just "xspf" -> True
    Just "wpl"  -> True
    _           -> False
  then return True
  else testfile to

shouldCopy :: FilePath -> FilePath -> IO Bool
shouldCopy from _ = case extension from of
  Just ext -> return (Set.notMember (T.toUpper ext) musicExtensions)
  _        -> return True

canCopy :: FilePath -> FilePath -> IO Bool
canCopy from _ = case extension from of
  Just ext -> return (Set.member (T.toLower ext) imageExtensions)
  _        -> return False

musicExtensions :: Set.Set Text
musicExtensions = Set.fromList
  [ "AMR"
  , "MP1"
  , "MP2"
  , "MP3"
  , "SPX"
  , "GSM"
  , "WMA"
  , "AAC"
  , "MPC"
  , "VQF"
  , "OTS"
  , "VOX"
  , "VOC"
  , "DWD"
  , "SMP"
  , "OGG"
  , "RA"
  , "RM"
  , "FLAC"
  , "LA"
  , "PAC"
  , "APE"
  , "OFR"
  , "OFS"
  , "OFF"
  , "RKA"
  , "SHN"
  , "TAK"
  , "TTA"
  , "WV"
  , "WMA"
  , "BRSTM"
  , "DTS"
  , "DTSHD"
  , "DTSMA"
  , "AST"
  , "AW"
  , "PSF"
  ]

imageExtensions :: Set.Set Text
imageExtensions = Set.fromList
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
  , "PI1"
  , "PI2"
  , "PI3"
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

convertOrCopy :: Opts -> (FilePath, FilePath) -> IO ()
convertOrCopy Opts{bitrate} (from, to) = do
  skip_ <- shouldSkip from to
  copy_ <- shouldCopy from to
  unless skip_ $ do
    copyOnly <-
      if copy_
      then return True
      else do
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
          ExitSuccess   -> return False
          ExitFailure _ -> return True
    can <- canCopy from to
    when
      (copyOnly && can)
      (procs "cp" [f2t from, f2t to] mempty)

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
  let files = map (\(absf, relf) -> (absf, output </> relf)) filesAndRels
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
    { message = ""
    , start = start0
    , now = start0
    , convprogress = Progress
      { progressDone = 0
      , progressTodo = toInteger (length files)
      }
    }

  withPool jobs $ \pool ->
    parallel_ pool (map (convertOrCopy opts >=> inc progRef) files)

