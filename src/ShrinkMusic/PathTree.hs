module ShrinkMusic.PathTree (PathTree, pathAsTree, F (..), fileTree) where

import Data.Map.Strict qualified as Map
import GHC.IsList (IsList (..))
import Streaming.Prelude qualified as S
import System.Directory.OsPath.FileType
import System.Directory.OsPath.Streaming

data PathTree
  = PT'Accept
  | PT'Node (Map OsPath PathTree)
  deriving stock (Show)

pattern PT'Empty :: PathTree
pattern PT'Empty = PT'Node Empty

instance IsList PathTree where
  type Item PathTree = OsPath
  fromList = foldMap pathAsTree
  toList = error "nope"

instance Semigroup PathTree where
  PT'Node l <> PT'Node r = PT'Node (Map.unionWith (<>) l r)
  _ <> _ = PT'Accept

instance Monoid PathTree where
  mempty = PT'Empty

type instance Index PathTree = [OsPath]

instance Contains PathTree where
  contains p = lens (getPath p) (setPath p)
    where
      getPath :: [OsPath] -> PathTree -> Bool
      getPath _ PT'Accept = True
      getPath (x : xs) (PT'Node m) = case m ^. at x of
        Just m' -> getPath xs m'
        Nothing -> False
      getPath _ _ = False

      setPath :: [OsPath] -> PathTree -> Bool -> PathTree
      setPath path t True = t <> splitPathAsTree path
      setPath path t False = prunePathTree (go path t)
        where
          go [] PT'Accept = PT'Empty
          go [] (PT'Node xs) = PT'Node xs
          go (x : xs) (PT'Node m) = PT'Node (m & at x % _Just %~ go xs)
          go _ _ = PT'Empty

--------------------------------------------------------------------------------

pathAsTree :: OsPath -> PathTree
pathAsTree = splitPathAsTree . splitDirectories

splitPathAsTree :: [OsPath] -> PathTree
splitPathAsTree [] = PT'Accept
splitPathAsTree (x : xs) = PT'Node (Map.singleton x (splitPathAsTree xs))

prunePathTree :: PathTree -> PathTree
prunePathTree (PT'Node m) = PT'Node (Map.filter isNotEmpty (Map.map prunePathTree m))
prunePathTree PT'Accept = PT'Accept

isNotEmpty :: PathTree -> Bool
isNotEmpty PT'Empty = False
isNotEmpty _ = True

--------------------------------------------------------------------------------

data F = F !OsPath ![OsString]

fileTree :: (MonadIO m) => OsPath -> Stream (Of F) m ()
fileTree root = stream root (splitDirectories root)
  where
    isFile = \case
      File -> True
      FileSym -> True
      _ -> False

    isDir = \case
      Directory -> True
      DirectorySym -> True
      _ -> False

    stream path pathComponents = do
      ft <- liftIO (getFileType path)
      if
        | isDir ft -> do
            ds <- liftIO (openDirStream path)
            let yieldPaths =
                  liftIO (readDirStream ds) >>= \case
                    Nothing -> pure []
                    Just p -> do
                      let nextComponents = pathComponents ++ [p]
                      let next = path </> p
                      ft <- liftIO (getFileType next)
                      if
                        | isFile ft -> S.yield (F next nextComponents) >> yieldPaths
                        | isDir ft -> ((next, nextComponents) :) <$> yieldPaths
                        | otherwise -> yieldPaths
            dirs <- yieldPaths
            liftIO (closeDirStream ds)
            mapM_ (uncurry stream) dirs
        | isFile ft -> S.yield (F path pathComponents)
        | otherwise -> pure ()
