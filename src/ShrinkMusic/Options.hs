{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module ShrinkMusic.Options (HasOptions, Options (..), getOptions, option) where

import Effectful.Reader.Static
import ShrinkMusic.PathTree
import Turtle.Options

data Options = Options
  { input, output :: OsPath
  , inputSplit, outputSplit :: [OsString]
  , bitrate :: Text
  , jobs :: Int
  , ignores :: PathTree
  , dryRun :: Bool
  , overwriteExisting :: Bool
  , convertAll :: Bool
  , outputFormat :: Text
  , outputExtension :: OsString
  , verbose :: Bool
  }

makeFieldLabelsNoPrefix ''Options

getOptions :: IO Options
getOptions =
  options "shrinkmusic" do
    input <- addTrailingPathSeparator <$> optOsPath "input" 'i' "Input directory"
    output <- addTrailingPathSeparator <$> optOsPath "output" 'o' "Output directory"
    bitrate <- optText "bitrate" 'b' "bitrate for audio"
    jobs <- optInt "jobs" 'j' "Number of jobs to use"
    ignores <- fromList <$> many (optOsPath "ignore" 'z' "Ignore these files/folders") <|> pure mempty
    dryRun <- switch "dry-run" 'd' "Do nothing; just output the plan"
    overwriteExisting <-
      switch
        "overwrite-existing"
        'X'
        "Overwrite existing files in the destination."
    convertAll <- switch "convert-all" 'C' "convert every single file"
    outputFormat <- optText "output-format" 'f' "format for the outputs"
    verbose <- switch "verbose" 'V' "verbose run"
    pure
      Options
        { inputSplit = splitDirectories input
        , outputSplit = splitDirectories output
        , outputExtension = case outputFormat of
            "mp3" -> "mp3"
            "flac" -> "flac"
            "ogg" -> "ogg"
            "opus" -> "ogg"
            fmt -> fromString (toString fmt)
        , ..
        }
  where
    optOsPath name char help = toOsPath' <$> optPath name char help

type HasOptions es = Reader Options @> es

option :: (HasOptions es) => Lens' Options a -> Eff es a
option l = asks (view l)
