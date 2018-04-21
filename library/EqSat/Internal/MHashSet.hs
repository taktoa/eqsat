--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Internal.MHashSet
  ( MHashSet
  , new
  , newWithCapacity
  , delete
  , member
  , insert
  , mapM_
  , forM_
  , foldM
  , computeOverhead
  , freeze
  , thaw
  ) where

--------------------------------------------------------------------------------

import           Prelude                 (Double, Eq, Show, flip, (<$>))

import           Control.Applicative     (pure)
import           Control.Monad           ((>>=))

import           Control.Monad.Primitive (PrimMonad (PrimState))

import           Data.Bool               (Bool)
import           Data.Int                (Int)

import           Data.Maybe              (isJust)

import           Data.Hashable           (Hashable)

import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as HashSet

import           EqSat.Internal.MHashMap (MHashMap)
import qualified EqSat.Internal.MHashMap as MHashMap

import           Flow                    ((.>))

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype MHashSet s k
  = MkMHashSet (MHashMap s k ())
  deriving ()

-- | FIXME: doc
new
  :: (PrimMonad m)
  => m (MHashSet (PrimState m) k)
  -- ^ FIXME: doc
new = MkMHashSet <$> MHashMap.new

-- | FIXME: doc
newWithCapacity
  :: (PrimMonad m)
  => Int
  -- ^ FIXME: doc
  -> m (MHashSet (PrimState m) k)
  -- ^ FIXME: doc
newWithCapacity capacity = do
  MkMHashSet <$> MHashMap.newWithCapacity capacity

-- | FIXME: doc
length
  :: (PrimMonad m)
  => MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> m Int
  -- ^ FIXME: doc
length (MkMHashSet mhm) = MHashMap.length mhm

-- | FIXME: doc
null
  :: (PrimMonad m)
  => MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
null (MkMHashSet mhm) = MHashMap.null mhm

-- | FIXME: doc
delete
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
delete (MkMHashSet hm) = MHashMap.delete hm

-- | FIXME: doc
member
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
member (MkMHashSet hm) k
  = isJust <$> MHashMap.lookup hm k

-- | FIXME: doc
insert
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
insert (MkMHashSet hm) k = MHashMap.insert hm k ()

-- | FIXME: doc
mapM_
  :: (PrimMonad m)
  => (k -> m any)
  -- ^ FIXME: doc
  -> MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
mapM_ f (MkMHashSet hm) = MHashMap.mapM_ (\k _ -> f k) hm

-- | FIXME: doc
forM_
  :: (PrimMonad m)
  => MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> (k -> m any)
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
forM_ = flip mapM_

-- | FIXME: doc
foldM
  :: (PrimMonad m)
  => MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> a
  -- ^ FIXME: doc
  -> (k -> a -> m a)
  -- ^ FIXME: doc
  -> m a
  -- ^ FIXME: doc
foldM (MkMHashSet hm) initial combiner
  = MHashMap.foldM hm initial (\k _ -> combiner k)

-- | FIXME: doc
computeOverhead
  :: (PrimMonad m)
  => MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> m Double
  -- ^ FIXME: doc
computeOverhead (MkMHashSet hm) = MHashMap.computeOverhead hm

-- | FIXME: doc
freeze
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashSet (PrimState m) k
  -- ^ FIXME: doc
  -> m (HashSet k)
  -- ^ FIXME: doc
freeze hs = foldM hs HashSet.empty (\k -> HashSet.insert k .> pure)

-- | FIXME: doc
thaw
  :: (Eq k, Hashable k, PrimMonad m)
  => HashSet k
  -- ^ FIXME: doc
  -> m (MHashSet (PrimState m) k)
  -- ^ FIXME: doc
thaw hs = do
  result <- newWithCapacity (HashSet.size hs)
  HashSet.foldr (\k x -> x >>= \() -> insert result k) (pure ()) hs
  pure result

--------------------------------------------------------------------------------
