{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prelude (
  module Optics,
  module Streaming,
  module Effectful,
  module Effectful.Log,
  type (@>),
  pattern Of,
  type Vector,
  module Control.Applicative,
  module Control.Arrow,
  module Control.Monad,
  module Control.Monad.Fail,
  module Control.Monad.Trans,
  module Data.Char,
  module Data.Fixed,
  module Data.Either,
  module Data.Foldable,
  module Data.Maybe,
  module Relude.Applicative,
  module Relude.Base,
  module Relude.Bool,
  module Relude.Container,
  module Relude.Debug,
  module Relude.DeepSeq,
  module Relude.Enum,
  module Relude.Exception,
  module Relude.Extra.Map,
  module Relude.Extra.Tuple,
  module Relude.Extra.Type,
  module Relude.File,
  module Relude.Foldable,
  module Relude.Function,
  module Relude.Functor,
  module Relude.Lifted,
  module Relude.List,
  module Relude.Monad.Either,
  module Relude.Monad.Maybe,
  module Relude.Monad.Trans,
  module Relude.Monoid,
  module Relude.Nub,
  module Relude.Numeric,
  module Relude.Print,
  module Relude.String,

  -- * OsPath
  module System.OsPath,
  toOsPath,
  toOsPath',
  fromOsPath,
  fromOsPath',

  -- * Monad util
  or_,
  and_,
) where

import Control.Applicative (Applicative)
import Control.Arrow ((&&&), (***))
import Control.Exception (throw)
import Control.Monad (
  Monad (return, (>>), (>>=)),
  MonadPlus (..),
  filterM,
  forever,
  join,
  liftM2,
  mapAndUnzipM,
  mfilter,
  replicateM,
  replicateM_,
  zipWithM,
  zipWithM_,
  (<$!>),
  (<=<),
  (=<<),
  (>=>),
 )
import Control.Monad.Fail
import Control.Monad.Trans (MonadTrans (..))
import Data.Char
import Data.Data
import Data.Either (Either (..), either, isLeft, isRight, lefts, partitionEithers, rights)
import Data.Fixed
import Data.Foldable (Foldable (maximum, minimum))
import Data.Maybe (
  Maybe (..),
  catMaybes,
  fromMaybe,
  isJust,
  isNothing,
  listToMaybe,
  mapMaybe,
  maybe,
  maybeToList,
 )
import Data.Vector (Vector)
import Effectful hiding (type (:>))
import Effectful qualified
import Effectful.Log
import Optics
import Optics.Label ()
import Relude.Applicative
import Relude.Base
import Relude.Bool
import Relude.Container
import Relude.Debug
import Relude.DeepSeq
import Relude.Enum
import Relude.Exception
import Relude.Extra.Map
import Relude.Extra.Tuple
import Relude.Extra.Type
import Relude.File
import Relude.Foldable
import Relude.Function
import Relude.Functor
import Relude.Lifted
import Relude.List hiding (uncons)
import Relude.Monad.Either
import Relude.Monad.Maybe
import Relude.Monad.Reexport
import Relude.Monad.Trans
import Relude.Monoid
import Relude.Nub
import Relude.Numeric
import Relude.Print
import Relude.String

import Streaming (Of, Stream)
import Streaming qualified
import System.OsPath hiding (combine, decodeUtf, encodeUtf)
import System.OsPath qualified as OsPath
import System.OsPath.Encoding (EncodingException (..))

-- | Prefix alias for '(Effectful.:>)' since that conflicts with 'Control.Lens.Cons'
type (@>) = (Effectful.:>)

pattern Of :: a -> b -> Of a b
pattern Of a b = a Streaming.:> b

or_ :: (Monad m) => [m Bool] -> m Bool
or_ [] = pure False
or_ (fx : fxs) = do
  x <- fx
  if x then pure True else or_ fxs

and_ :: (Monad m) => [m Bool] -> m Bool
and_ [] = pure True
and_ (fx : fxs) = do
  x <- fx
  if x then and_ fxs else pure False

instance IsString OsPath where
  fromString = toOsPath'

toOsPath' :: FilePath -> OsPath
toOsPath' = either throw id . toOsPath

fromOsPath' :: OsPath -> FilePath
fromOsPath' = either throw id . fromOsPath

toOsPath :: FilePath -> Either EncodingException OsPath
toOsPath = over _Left unwrapEncodingException . OsPath.encodeUtf

fromOsPath :: OsPath -> Either EncodingException FilePath
fromOsPath = over _Left unwrapEncodingException . OsPath.decodeUtf

instance ToText OsPath where
  toText = toText . fromOsPath'

unwrapEncodingException :: SomeException -> EncodingException
unwrapEncodingException e = case fromException e of
  Just ee -> ee
  Nothing -> throw e
