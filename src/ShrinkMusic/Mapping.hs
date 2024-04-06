{-# LANGUAGE Strict #-}
{-# LANGUAGE NoFieldSelectors #-}

module ShrinkMusic.Mapping where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Effectful.Error.Static
import ShrinkMusic.FileFormats
import ShrinkMusic.Options
import ShrinkMusic.PathMunge
import ShrinkMusic.PathTree (F (..), fileTree)
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory.OsPath.FileType
import System.Directory.OsPath.Streaming

data Mapping
  = Copy {from, to :: OsPath}
  | Convert {from, to :: OsPath}
  deriving stock (Show, Eq, Ord)

newtype TrySplitCueFlacPair = TrySplitCueFlacPair {cue :: OsPath}
  deriving stock (Show, Eq, Ord)

data SplitCueFlacPair = SplitCueFlacPair {cue, flac, to :: OsPath}
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''Mapping

data MappingError
  = CouldNotExtractPrefix OsPath
  | UserIgnored OsPath
  | IgnoreDotPaths OsPath
  | IgnorePlaylists OsPath
  | IgnoreUnknown OsPath
  | EmptyPath OsPath
  deriving stock (Show, Eq, Ord)

decideMappings :: forall es. (HasOptions es, Log @> es, IOE @> es) => Eff es ()
decideMappings = do
  root <- option #input
  pass1 <- S.toList_ (S.mapM (baseMapping @es) (fileTree root) `S.for` mapM_ S.yield)
  let (cues', maps) = partitionEithers pass1
  let byFrom x = ((x :: Mapping) ^. #from, x)
  let mapsByFrom = Map.fromList (map byFrom maps)
  let (cues, maps) = secondPass mapsByFrom cues'
  mapM_ (logInfo_ . show) cues
  mapM_ (logInfo_ . show) maps

mapping1
  :: ( HasOptions es
     , Error MappingError @> es
     , Error TrySplitCueFlacPair @> es
     )
  => F
  -> Eff es Mapping
mapping1 (F from split) = do
  ignores <- option #ignores
  output <- option #output
  outputS <- option #outputSplit
  inputS <- option #inputSplit
  convertAll <- option #convertAll
  outputFormat <- option #outputFormat
  outputExtension <- option #outputExtension
  errIf EmptyPath (null split)
  let filename = List.last split
  let (basename, ext) = splitExtension filename
  errIf UserIgnored (ignores ^. contains split)
  errIf IgnoreDotPaths (any (("." `Text.isInfixOf`) . toText) split)
  errIf IgnorePlaylists (ext `Set.member` playlistExts)
  copyPath <- case List.stripPrefix inputS split of
    Just relpath -> pure (joinPath (outputS ++ map mapBadChars relpath))
    Nothing -> throwError (CouldNotExtractPrefix from)
  let ~convertPath = replaceExtension copyPath outputExtension
  if
    | ext == "cue" -> throwError TrySplitCueFlacPair {cue = from}
    | ext `Set.member` imageExts && Set.member basename coverImageFiles -> pure Copy {from, to = copyPath}
    | ext `Set.notMember` musicExts -> throwError (IgnoreUnknown copyPath)
    | convertAll -> pure Convert {from, to = convertPath}
    | otherwise -> do
        shouldConvert <- determineIfShouldConvert from
        pure $! if shouldConvert then Copy {from, to = copyPath} else Convert {from, to = convertPath}
  where
    errIf f cond = when cond (throwError (f from))

baseMapping :: (HasOptions es, Log @> es) => F -> Eff es (Maybe (Either TrySplitCueFlacPair Mapping))
baseMapping fp =
  runErrorNoCallStack @MappingError (runErrorNoCallStack @TrySplitCueFlacPair (mapping1 fp)) >>= \case
    Left e -> Nothing <$ logInfo_ (show e)
    Right x -> pure (Just x)

secondPass :: Map OsPath Mapping -> [TrySplitCueFlacPair] -> ([SplitCueFlacPair], [Mapping])
secondPass mappings cues = (cueSplits, Map.elems nonCueFlacMappings)
  where
    nonCueFlacMappings = Map.filterWithKey (\k _ -> k `Map.notMember` foundPairs) mappings
    cueSplits = map (\(flac, (cue, to)) -> SplitCueFlacPair {cue, flac, to}) foundPairsList
    foundPairs = Map.fromList foundPairsList
    foundPairsList = do
      TrySplitCueFlacPair cuePath <- cues
      Convert {from = flacPath, to} <- mappings ^.. at (replaceExtension cuePath "flac") % _Just
      pure (flacPath, (cuePath, takeDirectory to))

determineIfShouldConvert :: OsPath -> Eff es Bool
determineIfShouldConvert path = pure False -- TODO
