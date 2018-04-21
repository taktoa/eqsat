--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Internal.MHashMap
  ( MHashMap
  , new
  , newWithCapacity
  , length
  , null
  , delete
  , lookup
  , member
  , insert
  , insertWith
  , mapM_
  , forM_
  , foldM
  , computeOverhead
  , freeze
  , thaw
  ) where

--------------------------------------------------------------------------------

import           Prelude
                 (Bool, Double, Num ((+), (-)), error, flip, ($))

import           Control.Applicative        (pure)
import           Control.Monad              (Monad ((>>=)), when)

import           Data.Eq                    (Eq ((==)))
import           Data.Functor               (fmap)
import           Data.Int                   (Int)
import           Data.Ord                   ((<))

import           Control.Monad.Primitive    (PrimMonad (PrimState), stToPrim)

import           Data.STRef                 (STRef)
import qualified Data.STRef                 as STRef

import           Data.Maybe                 (Maybe (Just, Nothing), isJust)

import           Data.Hashable              (Hashable)

import qualified Data.HashMap.Mutable.Basic as MHM

import qualified Data.HashMap.Strict        as HM

import           Flow                       ((.>))

--------------------------------------------------------------------------------

-- | FIXME: doc
data MHashMap s k v
  = UnsafeMkMHashMap
    { _mhashmapSizeRef    :: !(STRef s Int)
    , _mhashmapUnderlying :: !(MHM.MHashMap s k v)
    }
  deriving ()

-- | FIXME: doc
new
  :: (PrimMonad m)
  => m (MHashMap (PrimState m) k v)
  -- ^ FIXME: doc
new = do
  sizeRef    <- stToPrim (STRef.newSTRef 0)
  underlying <- MHM.new
  pure (UnsafeMkMHashMap sizeRef underlying)

-- | FIXME: doc
newWithCapacity
  :: (PrimMonad m)
  => Int
  -- ^ FIXME: doc
  -> m (MHashMap (PrimState m) k v)
  -- ^ FIXME: doc
newWithCapacity capacity = do
  sizeRef    <- stToPrim (STRef.newSTRef 0)
  underlying <- MHM.newSized capacity
  pure (UnsafeMkMHashMap sizeRef underlying)

-- | FIXME: doc
length
  :: (PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> m Int
  -- ^ FIXME: doc
length (UnsafeMkMHashMap sr _) = stToPrim (STRef.readSTRef sr)

-- | FIXME: doc
null
  :: (PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
null = length .> fmap (== 0)

-- | FIXME: doc
delete
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
delete (mhm@(UnsafeMkMHashMap sr hm)) k = do
  wasMember <- member mhm k
  when wasMember $ do
    modifySize mhm (\x -> x - 1)
  MHM.delete hm k

-- | FIXME: doc
lookup
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> m (Maybe v)
  -- ^ FIXME: doc
lookup (UnsafeMkMHashMap _ hm) = MHM.lookup hm

-- | FIXME: doc
member
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
member hm = lookup hm .> fmap isJust

-- | FIXME: doc
insert
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> v
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
insert mhm k v = insertWith mhm k v (\_ new -> pure new)

-- | FIXME: doc
insertWith
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> v
  -- ^ FIXME: doc
  -> (v -> v -> m v)
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
insertWith (mhm@(UnsafeMkMHashMap _ hm)) k v combiner = do
  modifySize mhm (\x -> x + 1)
  value <- lookup mhm k
           >>= (\case (Just old) -> do modifySize mhm (\x -> x - 1)
                                       combiner old v
                      Nothing    -> pure v)
  MHM.insert hm k value

-- | FIXME: doc
mapM_
  :: (PrimMonad m)
  => (k -> v -> m any)
  -- ^ FIXME: doc
  -> MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
mapM_ f (UnsafeMkMHashMap _ hm) = MHM.mapM_ f hm

-- | FIXME: doc
forM_
  :: (PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> (k -> v -> m any)
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
forM_ = flip mapM_

-- | FIXME: doc
foldM
  :: (PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> a
  -- ^ FIXME: doc
  -> (k -> v -> a -> m a)
  -- ^ FIXME: doc
  -> m a
  -- ^ FIXME: doc
foldM (UnsafeMkMHashMap _ hm) initial combine
  = MHM.foldM (\x k v -> combine k v x) initial hm

-- | FIXME: doc
computeOverhead
  :: (PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> m Double
  -- ^ FIXME: doc
computeOverhead (UnsafeMkMHashMap _ hm) = MHM.computeOverhead hm

-- | FIXME: doc
freeze
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> m (HM.HashMap k v)
  -- ^ FIXME: doc
freeze hm = do
  let collision = error "MHashMap.freeze: collision occurred! this is a bug."
  foldM hm HM.empty
    $ \k v -> HM.insertWith (\_ _ -> collision) k v .> pure

-- | FIXME: doc
thaw
  :: (Eq k, Hashable k, PrimMonad m)
  => HM.HashMap k v
  -- ^ FIXME: doc
  -> m (MHashMap (PrimState m) k v)
  -- ^ FIXME: doc
thaw hm = do
  let fold :: (Monad m) => HM.HashMap k v -> a -> (k -> v -> a -> m a) -> m a
      fold hmap initial combine
        = HM.foldrWithKey (\k v x -> x >>= combine k v) (pure initial) hmap
  result <- newWithCapacity (HM.size hm)
  fold hm () $ \k v () -> insert result k v
  pure result

--------------------------------------------------------------------------------

-- Helper functions

modifySize
  :: (PrimMonad m)
  => MHashMap (PrimState m) k v
  -> (Int -> Int)
  -> m ()
modifySize (UnsafeMkMHashMap sr _) f = stToPrim $ do
  STRef.modifySTRef sr
    $ \old -> let new = f old
              in if new < 0
                 then error "modifySize: assertion violated"
                 else new

--------------------------------------------------------------------------------
