--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------

module EqSat.Tests.Gen.Misc
  ( vector
  , mvector
  ) where

--------------------------------------------------------------------------------

import qualified Hedgehog                    as HH
import qualified Hedgehog.Gen                as HH.G
import qualified Hedgehog.Range              as HH.R

import           Control.Monad.Primitive     (PrimMonad (PrimState))

import           Data.Vector.Generic         (Mutable, Vector)
import qualified Data.Vector.Generic         as Vector
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MVector

import           Data.Proxy                  (Proxy (Proxy))

--------------------------------------------------------------------------------

-- | FIXME: doc
vector
  :: (HH.MonadGen m, Vector v a)
  => Proxy v
  -> HH.Range Int
  -> m a
  -> m (v a)
vector _ range gen = Vector.fromList <$> HH.G.list range gen

-- | FIXME: doc
mvector
  :: (HH.MonadGen m, PrimMonad m, Vector v a)
  => Proxy v
  -> HH.Range Int
  -> m a
  -> m (Mutable v (PrimState m) a)
mvector p range gen = vector p range gen >>= Vector.thaw

--------------------------------------------------------------------------------
