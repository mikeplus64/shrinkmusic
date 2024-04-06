main :: IO ()
main = pure ()

-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TupleSections #-}

-- import Control.Applicative
-- import Control.Arrow ((&&&))
-- import Control.Concurrent.ParallelIO.Local
-- import Control.Exception (Exception, bracket, bracket_, finally, mask, throwIO)
-- import Control.Foldl qualified as F
-- import Control.Monad (filterM, forM, forM_)
-- import Control.Monad.State.Strict
-- import Data.List (
--   foldl',
--   sort,
--  )
-- import Data.Map.Strict (Map)
-- import Data.Map.Strict qualified as Map
-- import Data.Set (Set)
-- import Data.Set qualified as Set
-- import Data.Text (Text)
-- import Data.Text qualified as T
-- import Data.Text.Lazy qualified as TL
-- import Data.Text.Read qualified as T
-- import Data.Time
-- import GHC.IO.Encoding (
--   setLocaleEncoding,
--   utf8,
--  )
-- import Streaming
-- import Streaming.ByteString qualified as S
-- import Streaming.Prelude qualified as S
-- import System.Directory qualified as Directory
-- import System.FilePath (replaceExtension)
-- import System.IO qualified as IO (
--   hSetEncoding,
--   stderr,
--   stdout,
--   utf8,
--  )
-- import System.IO.Temp (withSystemTempDirectory)
-- import System.Posix (closeDirStream, openDirStream, readDirStream, touchFile)
-- import System.ProgressBar
-- import Text.Printf qualified as P
-- import Turtle hiding (parallel, sort, testfile)
-- import Turtle qualified
-- import Prelude hiding (FilePath)

-- data Opts = Opts
--   { input :: !FilePath
--   , output :: !FilePath
--   , bitrate :: !Text
--   , jobs :: !Int
--   , ignores :: !(Set FilePath)
--   , dryRun :: !Bool
--   , overwriteExisting :: !Bool
--   , convertAll :: !Bool
--   , outputFormat :: !Text
--   , verbose :: !Bool
--   }
--   deriving (Show)

-- f2t :: FilePath -> Text
-- f2t = T.pack

-- data Mapping
--   = Copy {from, to :: !FilePath}
--   | Convert {from, to :: !FilePath}
--   deriving (Show, Eq, Ord)

-- data Splitter = Splitter
--   { cue :: !Mapping
--   , flac :: !Mapping
--   , toDir :: !FilePath
--   }
--   deriving (Show, Eq, Ord)

-- data Action
--   = Map !Mapping
--   | Split !Splitter
--   deriving (Show, Eq, Ord)

-- mkTestExt :: Maybe Text -> Set.Set Text -> a -> a -> a
-- mkTestExt thing exts y n = case thing of
--   Just ext | Set.member (T.toLower ext) exts -> y
--   _ -> n

-- decideMappings :: Opts -> FilePath -> IO [Mapping]
-- decideMappings Opts {input, output, ignores, convertAll, outputFormat} from =
--   respectIgnores
--     . ignoreDot from
--     . ignoreDot (filename from)
--     . ignorePlaylists
--     . ( if convertAll
--           then convertMusic
--           else convert320mp3dir . copyMusic . convertMusic
--       )
--     . copyCue
--     . copyImages
--     $ return []
--   where
--     finish :: a -> b -> IO a
--     finish x _ = return x

--     continue :: IO a -> IO a
--     continue = id

--     respectIgnores = if Set.member from ignores then finish [] else continue

--     testExt exts r = mkTestExt ext exts (finish r) continue
--     ext = T.pack <$> Turtle.extension from
--     dir = T.toLower (f2t (Turtle.directory from))
--     base = T.toLower (f2t (Turtle.basename from))
--     to = case Turtle.stripPrefix input from of
--       Just relpath -> output </> mapBadChars relpath
--       Nothing -> error $ "could not extract prefix from " ++ show from

--     ignoreDot ('.' : _) = finish []
--     ignoreDot _ = continue

--     ignorePlaylists = testExt playlistExts []

--     bitrate320dir =
--       contains (choice [("(" <|> "[" <|> "@") >> bitrate320])
--         <|> suffix bitrate320

--     bitrate320 = do
--       _ <- "320"
--       option $ choice ["kbps", "kps", "kbs", "k"]

--     conversion = Convert {from, to = replaceExtension to (T.unpack (outputExtension outputFormat))}

--     copy = Copy {from, to}

--     convert320mp3dir =
--       if ext == Just "mp3" && not (null (match bitrate320dir dir))
--         then finish [conversion]
--         else continue

--     convertMusic = testExt musicExts [conversion]
--     copyMusic = testExt keepMusicExts [copy]

--     copyImages =
--       if maybe False (`Set.member` imageExts) ext
--         && Set.member base coverImageFiles
--         then finish [copy]
--         else continue

--     copyCue = if ext == Just "cue" then finish [copy] else continue

-- outputExtension :: Text -> Text
-- outputExtension format = case format of
--   "mp3" -> "mp3"
--   "flac" -> "flac"
--   "ogg" -> "ogg"
--   "opus" -> "ogg"
--   _ -> error "unknown format"

-- splitCues :: Set Mapping -> Set Action
-- splitCues mappings =
--   Set.map
--     Map
--     ( Set.filter
--         (\m -> extension (to m) /= Just "cue")
--         (mappings Set.\\ foundPairs)
--     )
--     `Set.union` Set.fromList (map Split splits)
--   where
--     flacsAndCueMaps :: Map FilePath Mapping
--     flacsAndCueMaps = Map.fromList $ do
--       m <- Set.toList mappings
--       Just ext <- [extension (from m)]
--       guard (ext == "cue" || ext == "flac")
--       return (from m, m)

--     isFlacMapping f = extension (from f) == Just "flac"

--     flacMaps :: Map FilePath Mapping
--     cueMaps :: Map FilePath Mapping
--     (flacMaps, cueMaps) = Map.partition isFlacMapping flacsAndCueMaps

--     splits =
--       [ Splitter {cue, flac, toDir = directory (to flac)}
--       | cue <- Map.elems cueMaps
--       , flac <- case Map.lookup (replaceExtension (from cue) "flac") flacMaps of
--           Just r -> [r]
--           _ -> []
--       ]

--     foundPairs = Set.fromList (concatMap (\s -> [cue s, flac s]) splits)

-- data ActionLogItem
--   = CopySuccess !Action
--   | ConvertSuccess !Action
--   | ConvertFail !Action
--   deriving stock (Show)

-- interpret :: Opts -> Action -> Stream (Of ActionLogItem) IO ()
-- interpret _ (Map Copy {from, to}) = do
--   void (proc "cp" [f2t from, f2t to] mempty)
-- interpret opts (Map Convert {from, to})
--   | extension from == Just "flac" && extension to == Just "flac" =
--       interpret
--         opts
--         (Map Copy {from, to})
-- interpret opts (Map Convert {from, to}) =
--   proc "ffmpeg" args mempty >>= \case
--     ExitSuccess -> return ()
--     ExitFailure _ -> interpret opts (Map Copy {from, to})
--   where
--     args = case outputFormat opts of
--       "mp3" ->
--         [ "-loglevel"
--         , "error"
--         , "-i"
--         , f2t from
--         , "-vn"
--         , "-codec:a"
--         , "libmp3lame"
--         , "-b:a"
--         , bitrate opts
--         , "-map_metadata"
--         , "0:g"
--         , "-n"
--         , f2t to
--         ]
--       "wma" ->
--         [ "-loglevel"
--         , "error"
--         , "-i"
--         , f2t from
--         , "-vn"
--         , "-codec:a"
--         , "wmav2"
--         , "-b:a"
--         , bitrate opts
--         , "-vbr"
--         , "on"
--         , "-map_metadata"
--         , "0:g"
--         , "-n"
--         , f2t to
--         ]
--       "flac" ->
--         [ "-loglevel"
--         , "error"
--         , "-i"
--         , f2t from
--         , "-vn"
--         , "-codec:a"
--         , "flac"
--         , "-map_metadata"
--         , "0:g"
--         , "-n"
--         , f2t to
--         ]
--       _ ->
--         -- default to opus
--         [ "-loglevel"
--         , "error"
--         , "-i"
--         , f2t from
--         , "-vn"
--         , "-codec:a"
--         , "libopus"
--         , "-b:a"
--         , bitrate opts
--         , "-vbr"
--         , "on"
--         , "-compression_level"
--         , "10"
--         , "-map_metadata"
--         , "0:g"
--         , "-n"
--         , f2t to
--         ]
-- interpret opts (Split Splitter {cue, flac, toDir}) =
--   withSystemTempDirectory "shrinkmusic" $ \tmpf -> do
--     -- Even if the cue file is left "dangling" at the end of this, we still
--     -- include it in the output, just so that the "evidence" of the splitting
--     -- remains, so shrinkmusic doesn't try to duplicate work on a second
--     -- invocation
--     --
--     -- Since media players might be confused by a cue file referring to a flac
--     -- that doesn't exist, we rename it to %f.cue.split, if and only if we succeed
--     -- in doing the cue split.
--     proc
--       "shnsplit"
--       [ "-f"
--       , f2t (from cue)
--       , "-o"
--       , "flac"
--       , f2t (from flac)
--       , "-t"
--       , "%n %t"
--       , "-d"
--       , T.pack tmpf
--       , "-q"
--       ]
--       mempty
--       >>= \case
--         ExitSuccess -> do
--           flacOuts <- lstree tmpf `Turtle.fold` F.list >>= filterM (testfile opts) --
--           cueOk <-
--             proc
--               "cuetag.sh"
--               (f2t (from cue) : sort (map f2t flacOuts))
--               mempty
--           case cueOk of
--             ExitSuccess -> return ()
--             ExitFailure _ ->
--               printf
--                 ("Warning: cuetag.sh failed for " % fp % "\n")
--                 (from cue)

--           interpret opts (Map cue {to = to cue})

--           mapM_
--             (interpret opts)
--             [ Map
--               Convert
--                 { from = out
--                 , to =
--                     toDir
--                       </> replaceExtension
--                         (mapBadChars (filename out))
--                         (T.unpack (outputExtension (outputFormat opts)))
--                 }
--             | out <- flacOuts
--             ]
--         ExitFailure _ -> do
--           -- flac is probably reasonably compressable when it's a giant file, less
--           -- gain to be had by converting one big flac rather than lots of littler
--           -- ones
--           interpret opts (Map cue)
--           interpret opts (Map flac)

-- data PlanPhase
--   = ReadingFiles
--   | FindingDeleted
--   | FilteringNonMusicalDirectories
--   | CreatingActions
--   | Done
--   deriving (Show, Eq, Ord, Enum, Bounded)

-- data ConvertProgress = ConvertProgress
--   { start :: {-# UNPACK #-} !UTCTime
--   , now :: {-# UNPACK #-} !UTCTime
--   }

-- testfile :: (MonadIO m) => Opts -> FilePath -> m Bool
-- testfile opts path = do
--   when (verbose opts) (printf ("testing file" % fp % "\n") path)
--   Turtle.testfile path

-- main :: IO ()
-- main = do
--   setLocaleEncoding utf8
--   IO.hSetEncoding IO.stdout IO.utf8
--   IO.hSetEncoding IO.stderr IO.utf8

--   opts0@Opts {input, output, jobs, dryRun, ignores, overwriteExisting, verbose} <-
--     options "shrinkmusic" $
--       Opts
--         <$> fmap (</> "") (optPath "input" 'i' "Input directory")
--         <*> fmap (</> "") (optPath "output" 'o' "Output directory")
--         <*> optText "bitrate" 'b' "bitrate for audio"
--         <*> optInt "jobs" 'j' "Number of jobs to use"
--         <*> ( Set.fromList
--                 `fmap` many (optPath "ignore" 'z' "Ignore this file")
--                 <|> pure Set.empty
--             )
--         <*> switch "dry-run" 'd' "Do nothing; just output the plan"
--         <*> switch
--           "overwrite-existing"
--           'X'
--           "Overwrite existing files in the destination."
--         <*> switch "convert-all" 'C' "convert every single file"
--         <*> optText "output-format" 'f' "format for the outputs"
--         <*> switch "verbose" 'V' "verbose run"

--   ignoreRecur <-
--     fmap (Set.fromList . concat) . forM (Set.toList ignores) $ \i -> do
--       isDir <- testdir i
--       if isDir
--         then do
--           yall <- lstree i `Turtle.fold` F.list
--           filterM (testfile opts0) yall
--         else return [i]

--   let opts = opts0 {ignores = ignoreRecur}

--   -- takes *all* files so we can copy everything over

--   putStrLn "constructing plan..."

--   when verbose $ print opts

--   planPP <-
--     newProgressBar
--       defStyle
--         { stylePrefix = Label \Progress {progressCustom} _ ->
--             TL.pack (show progressCustom)
--         }
--       15
--       Progress
--         { progressDone = fromIntegral (fromEnum (minBound :: PlanPhase))
--         , progressTodo = fromIntegral (fromEnum (maxBound :: PlanPhase))
--         , progressCustom = minBound :: PlanPhase
--         }

--   let
--     planProgress :: PlanPhase -> IO ()
--     planProgress pp = do
--       print pp
--       updateProgress planPP \prog -> prog {progressDone = fromIntegral (fromEnum pp), progressCustom = pp}

--   planProgress ReadingFiles
--   inFiles <- lstree input `Turtle.fold` F.list >>= filterM (testfile opts)
--   mappings <- concat <$> mapM (decideMappings opts) inFiles

--   when verbose $ print opts

--   let targets = Set.fromList (map to mappings)
--   -- find where files were deleted from original folder i.e., the files in the
--   -- destination folder that are not accounted for by the action plan
--   planProgress FindingDeleted

--   -- pre-existing files in the output directory
--   destfiles <-
--     Set.fromList <$> (lstree output `Turtle.fold` F.list >>= filterM (testfile opts))
--   let !deletes =
--         -- what _is_ in destination that is not in targets
--         destfiles `Set.difference` targets

--   when verbose $ do
--     putStrLn "deletions: "
--     forM_ deletes print

--   -- find directories that have no music in them (e.g., they're cover image
--   -- scans; stuff i don't care about for my phone)
--   planProgress FilteringNonMusicalDirectories

--   let dirs2targets =
--         Map.fromListWith
--           Set.union
--           (map ((directory &&& Set.singleton) . to) mappings)
--   let isMusicOrSubdir p =
--         mkTestExt (T.pack <$> extension p) musicExts True False || directory p == p
--   let !nonmusicdirs = Map.filter (not . any isMusicOrSubdir) dirs2targets

--   planProgress CreatingActions

--   actions <-
--     Set.toList . splitCues . Set.fromList <$> withPool
--       jobs
--       \pool ->
--         parallelFilterM
--           pool
--           ( \p ->
--               if overwriteExisting
--                 then pure True
--                 else
--                   not
--                     <$> or_
--                       [ -- it already exists, and they're the same file (for copy actions)
--                         and_
--                           [ testfile opts (to p) -- dest file already exists...
--                           , -- and...
--                             -- - file sizes are the same (used as proxy for file equality)
--                             --   if it's a copy action; or
--                             -- - the file just already exists for a convert action
--                             or_
--                               [ case p of
--                                   Copy {from, to} -> do
--                                     fs <- du from
--                                     ts <- du to
--                                     when
--                                       (fs /= ts)
--                                       (printf ("mismatch " % fp % " from " % fp % "\n") to from)
--                                     pure (fs == ts)
--                                   _ -> pure False
--                               , case p of
--                                   Convert {from, to} -> pure True
--                                   _ -> pure False
--                               ]
--                           ]
--                       , -- or, the directory is not a music directory
--                         pure (directory (to p) `Map.member` nonmusicdirs)
--                       ]
--           )
--           mappings

--   planProgress Done

--   start0 <- date
--   if dryRun
--     then do
--       forM_ actions $ \case
--         Map Copy {to} -> printf ("copy " % fp % "\n") to
--         Map Convert {to} -> printf ("convert " % fp % "\n") to
--         Split Splitter {cue} -> printf ("split " % fp % "\n") (from cue)

--       let copies =
--             filter
--               ( \case
--                   Map Copy {} -> True
--                   _ -> False
--               )
--               actions
--       let converts =
--             filter
--               ( \case
--                   Map Convert {} -> True
--                   _ -> False
--               )
--               actions
--       let splits =
--             filter
--               ( \case
--                   Split {} -> True
--                   _ -> False
--               )
--               actions
--       let sizeOf f =
--             foldl' (+) 0
--               <$> mapM
--                 du
--                 ( do
--                     a <- f
--                     case a of
--                       Map m -> [from m]
--                       Split Splitter {flac, cue} -> [from flac, from cue]
--                 )

--       totalInputFileSize <- sizeOf actions
--       totalCopySize <- sizeOf copies
--       totalConvertSize <- sizeOf converts
--       totalSplitSize <- sizeOf splits

--       printf "\nSUMMARY\n"
--       printf
--         ("Actions:   \t" % d % "\t" % sz % "\n")
--         (length actions)
--         totalInputFileSize
--       printf
--         ("Converts:  \t" % d % "\t" % sz % "\n")
--         (length converts)
--         totalConvertSize
--       printf
--         ("Copies:    \t" % d % "\t" % sz % "\n")
--         (length copies)
--         totalCopySize
--       printf
--         ("Cue splits:\t" % d % "\t" % sz % "\n")
--         (length splits)
--         totalSplitSize
--     else do
--       putStrLn "making directory tree..."
--       mapM_
--         ( \case
--             Map m -> mktree (directory (to m))
--             Split s -> mktree (toDir s)
--         )
--         actions
--       unless (Set.null deletes) $ do
--         putStrLn "running deletes..."
--         mapM_ rm deletes
--       putStrLn "converting..."
--       convertPP <-
--         newProgressBar
--           defStyle
--             { stylePostfix = Label
--                 \Progress {progressDone, progressTodo, progressCustom = ConvertProgress {start, now}} _ ->
--                   let
--                     elapsed, avg, remain :: Double
--                     elapsed = realToFrac (diffUTCTime now start)
--                     avg = elapsed / fromIntegral progressDone
--                     remain = avg * fromIntegral (progressTodo - progressDone)
--                    in
--                     TL.pack
--                       (P.printf "%.2fs elapsed. %0.2fs remaining" elapsed remain)
--             }
--           15
--           Progress
--             { progressCustom = ConvertProgress {start = start0, now = start0}
--             , progressDone = 0
--             , progressTodo = fromIntegral (length actions)
--             }

--       let step :: Action -> IO ()
--           step action = do
--             interpret opts action
--             now <- date
--             updateProgress
--               convertPP
--               \c ->
--                 c
--                   { progressCustom = (progressCustom c) {now}
--                   , progressDone = progressDone c + 1
--                   }

--       withPool jobs \pool -> parallel_ pool (map step actions)

-- --------------------------------------------------------------------------------

-- parallelFilterM :: Pool -> (a -> IO Bool) -> [a] -> IO [a]
-- parallelFilterM pool test xs =
--   map snd . filter fst
--     <$> parallel
--       pool
--       [ test x <&> (,x)
--       | x <- xs
--       ]
