{-# LANGUAGE NoFieldSelectors #-}

module ShrinkMusic.PathTree (PathTree (..), pathAsTree, Tree (..), TreePath (..), getPathTree, overPathTree) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import GHC.IsList (IsList (..))
import Streaming.Prelude qualified as S
import System.Directory.OsPath.FileType
import System.Directory.OsPath.Streaming
import Text.Show (Show (showsPrec))

data PathTree
  = PT'Accept
  | PT'Node (Map OsPath PathTree)
  deriving stock (Show)

pattern PT'Empty :: PathTree
pattern PT'Empty = PT'Node Empty

instance Semigroup PathTree where
  PT'Node l <> PT'Node r = PT'Node (Map.unionWith (<>) l r)
  _ <> _ = PT'Accept

instance Monoid PathTree where
  mempty = PT'Empty

type instance Index PathTree = [OsPath]

data TreePath = TreePath
  { path :: !OsPath
  , components :: ![OsPath]
  , filename :: ~OsString
  , basename :: ~OsString
  , extension :: ~OsString
  }

makeFieldLabelsNoPrefix ''TreePath

instance Eq TreePath where
  a == b = a ^. #path == b ^. #path

instance Ord TreePath where
  a `compare` b = (a ^. #path) `compare` (b ^. #path)

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

instance IsList PathTree where
  type Item PathTree = OsPath
  fromList = foldMap pathAsTree
  toList = error "nope"

--------------------------------------------------------------------------------

data Tree m = Tree
  { root :: !TreePath
  , subdirs :: !(Vector (m (Tree m)))
  , files :: !(Vector TreePath)
  }

getPathTree :: forall m. (MonadIO m) => OsPath -> m (Tree m)
getPathTree root = do
  ftype <- liftIO (getFileType root)
  unless (isDir ftype) (error "tree: Must be a directory")
  let rootTP = mkTreePath root (fromList (splitDirectories root))
  let stream = case treeStream (rootTP, ftype) of
        Right d -> d
        _ -> error "tree: Must be a directory"
  go rootTP stream
  where
    go :: TreePath -> TreeStream m -> m (Tree m)
    go dir TSEmpty = pure Tree {root = dir, subdirs = mempty, files = mempty}
    go _ (TSDir dir subStream) = do
      (moreFiles, moreDirs) <- partitionEithers <$> S.toList_ subStream
      pure
        Tree
          { root = dir
          , subdirs = fromList (map (go dir) moreDirs)
          , files = fromList moreFiles
          }

overPathTree :: (MonadIO m) => OsPath -> (TreePath -> Vector TreePath -> m ()) -> m ()
overPathTree root0 overDir = liftIO (getPathTree root0) >>= go
  where
    go Tree {root, subdirs, files} = do
      overDir root files
      mapM_
        ( \sd -> do
            subtree <- liftIO sd
            go subtree
        )
        subdirs

--------------------------------------------------------------------------------

mkTreePath :: OsPath -> [OsPath] -> TreePath
mkTreePath _ Empty = error "empty components for treepath"
mkTreePath path components = TreePath {path, components, filename, basename, extension}
  where
    filename = List.last components
    ~(basename, extension) = splitExtension filename

instance Show TreePath where
  showsPrec p TreePath {path} = showsPrec p path

type TreeStreamItem m = Either TreePath (TreeStream m)
data TreeStream m
  = TSDir !TreePath !(Stream (Of (TreeStreamItem m)) m ())
  | TSEmpty

treeStream :: (MonadIO m) => (TreePath, FileType) -> TreeStreamItem m
treeStream (tp, ftype)
  | isDir ftype = Right (TSDir tp (streamDir tp))
  | isFile ftype = Left tp
  | otherwise = Right TSEmpty
  where
    streamDir TreePath {path = dir, components = dirS} = do
      S.unfoldr
        ( \ds ->
            liftIO (readDirStream ds) >>= \case
              Nothing -> Left () <$ liftIO (closeDirStream ds)
              Just item -> do
                let path = dir </> item
                ftype <- liftIO (getFileType path)
                let next = mkTreePath item (dirS :> item)
                pure (Right (treeStream (next, ftype), ds))
        )
        =<< liftIO (openDirStream dir)

isFile, isDir :: FileType -> Bool
isFile = \case
  File -> True
  FileSym -> True
  _ -> False
isDir = \case
  Directory -> True
  DirectorySym -> True
  _ -> False
