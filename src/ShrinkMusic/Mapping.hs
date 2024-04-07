{-# LANGUAGE Strict #-}
{-# LANGUAGE NoFieldSelectors #-}

module ShrinkMusic.Mapping where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Effectful.Error.Static
import ShrinkMusic.FileFormats
import ShrinkMusic.Options
import ShrinkMusic.PathMunge
import ShrinkMusic.PathTree
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory.OsPath.FileType
import System.Directory.OsPath.Streaming

--------------------------------------------------------------------------------

data Action
  = Copy {from :: TreePath, to :: OsPath}
  | Convert {from :: TreePath, to :: OsPath}
  | CueSplitAction {cue, flac :: TreePath, to :: OsPath}
  deriving stock (Show, Eq, Ord)

newtype MappingError = CouldNotExtractPrefix [OsPath]
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''Action

data TreePathCheck = TreePathCheck
  { tp :: TreePath
  , isImage :: Bool
  , isMusic :: Bool
  , isCue :: Bool
  }

makeFieldLabelsNoPrefix ''TreePathCheck

--------------------------------------------------------------------------------

actions :: (HasOptions es, Log @> es, IOE @> es) => OsPath -> Stream (Of Action) (Eff es) ()
actions root = streamVector do
  overPathTree root (\path files -> lift (dirMappings path files) >>= S.yield)

--------------------------------------------------------------------------------

dirMappings
  :: forall es
   . (HasOptions es, Log @> es)
  => TreePath
  -> Vector TreePath
  -> Eff es (Vector Action)
dirMappings dir files0 = do
  inputS <- option #inputSplit
  outputS <- option #outputSplit
  outputExtension <- option #outputExtension
  ignores <- option #ignores
  let
    files :: Vector TreePathCheck
    files =
      files0
        & Vector.filter (\TreePath {components} -> not (ignores ^. contains components))
        <&> \tp@TreePath {extension = ext, basename} ->
          TreePathCheck
            { tp
            , isCue = ext == ".cue"
            , isMusic = ext `Set.member` musicExts
            , isImage = ext `Set.member` imageExts && basename `Set.member` coverImageFiles
            }

    test check tpc@TreePathCheck {tp} = tp <$ guard (tpc ^. check)
    fileNameMap vec = Map.fromList [(filename, f) | f@TreePath {filename} <- toList vec]
    musicFiles = files & Vector.mapMaybe (test #isMusic) & fileNameMap
    imageFiles = files & Vector.mapMaybe (test #isImage)
    cueFiles = files & Vector.mapMaybe (test #isCue)

    outputFor :: [OsString] -> Either MappingError OsPath
    outputFor inpathS = case List.stripPrefix (toList inputS) (toList inpathS) of
      Just relpath -> Right (joinPath (outputS ++ map mapBadChars relpath))
      Nothing -> Left (CouldNotExtractPrefix inpathS)

    cueErrs :: [MappingError]
    cueFlacs :: [Action]
    (cueErrs, cueFlacs) = partitionEithers do
      cue <- toList cueFiles
      flac <- musicFiles ^.. at ((cue ^. #basename) <.> "flac") % _Just
      let outDir = outputFor (List.init (flac ^. #components))
      let mkCueSplit to = CueSplitAction {cue, flac, to}
      pure (outDir <&> mkCueSplit)

    cueFlacNames :: Set OsString
    cueFlacNames = Set.fromList (mapMaybe (^? #flac % #filename) cueFlacs)

    convertErrs :: [MappingError]
    converts :: [Action]
    (convertErrs, converts) =
      partitionEithers $
        toList (Map.filterWithKey (\k _ -> k `Set.notMember` cueFlacNames) musicFiles)
          <&> \from ->
            outputFor (from ^. #components) <&> \out ->
              Convert {from, to = replaceExtension out outputExtension}

    copiesErrs :: [MappingError]
    copies :: [Action]
    (copiesErrs, copies) =
      partitionEithers $
        toList imageFiles <&> \from ->
          outputFor (from ^. #components) <&> \to -> Copy {from, to}

  forM_ cueErrs (logAttention_ . show)
  forM_ convertErrs (logAttention_ . show)
  forM_ copiesErrs (logAttention_ . show)

  pure (fromList (cueFlacs ++ converts ++ copies))

--------------------------------------------------------------------------------

streamVector :: (Monad m) => Stream (Of (Vector a)) m () -> Stream (Of a) m ()
streamVector = flip S.for (mapM_ S.yield)
