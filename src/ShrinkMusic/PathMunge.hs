module ShrinkMusic.PathMunge where

import Data.Set qualified as Set
import System.OsPath qualified as OS

data Rename = Rename

bads :: Set Char
bads = fromList "\"'?<>\\|:"

mapBadChars :: OsString -> OsPath
mapBadChars = OS.pack . concatMap (map OS.unsafeFromChar . unbad . OS.toChar) . OS.unpack
  where
    unbad :: Char -> [Char]
    unbad '|' = ")"
    unbad ':' = "-"
    unbad c = [c | c `Set.notMember` bads]
