module ShrinkMusic.Interpret where

import ShrinkMusic.Mapping

interpret :: Opts -> Action -> Stream (Of ActionLogItem) IO ()
interpret _ (Map Copy {from, to}) = do
  void (proc "cp" [f2t from, f2t to] mempty)
interpret opts (Map Convert {from, to})
  | extension from == Just "flac" && extension to == Just "flac" =
      interpret
        opts
        (Map Copy {from, to})
interpret opts (Map Convert {from, to}) =
  proc "ffmpeg" args mempty >>= \case
    ExitSuccess -> return ()
    ExitFailure _ -> interpret opts (Map Copy {from, to})
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
      _ ->
        -- default to opus
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
interpret opts (Split Splitter {cue, flac, toDir}) =
  withSystemTempDirectory "shrinkmusic" $ \tmpf -> do
    -- Even if the cue file is left "dangling" at the end of this, we still
    -- include it in the output, just so that the "evidence" of the splitting
    -- remains, so shrinkmusic doesn't try to duplicate work on a second
    -- invocation
    --
    -- Since media players might be confused by a cue file referring to a flac
    -- that doesn't exist, we rename it to %f.cue.split, if and only if we succeed
    -- in doing the cue split.
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
      , T.pack tmpf
      , "-q"
      ]
      mempty
      >>= \case
        ExitSuccess -> do
          flacOuts <- lstree tmpf `Turtle.fold` F.list >>= filterM (testfile opts) --
          cueOk <-
            proc
              "cuetag.sh"
              (f2t (from cue) : sort (map f2t flacOuts))
              mempty
          case cueOk of
            ExitSuccess -> return ()
            ExitFailure _ ->
              printf
                ("Warning: cuetag.sh failed for " % fp % "\n")
                (from cue)

          interpret opts (Map cue {to = to cue})

          mapM_
            (interpret opts)
            [ Map
              Convert
                { from = out
                , to =
                    toDir
                      </> replaceExtension
                        (mapBadChars (filename out))
                        (T.unpack (outputExtension (outputFormat opts)))
                }
            | out <- flacOuts
            ]
        ExitFailure _ -> do
          -- flac is probably reasonably compressable when it's a giant file, less
          -- gain to be had by converting one big flac rather than lots of littler
          -- ones
          interpret opts (Map cue)
          interpret opts (Map flac)
