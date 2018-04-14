--------------------------------------------------------------------------------

-- | FIXME: doc
module MHashMap
  ( MHM.MHashMap
  , MHM.new
  , MHM.newSized
  , MHM.delete
  , MHM.lookup
  , MHM.insert
  , insertWith
  , MHM.mapM_
  , foldM
  , MHM.computeOverhead
  , freeze
  , thaw
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.Primitive

import           Data.Hashable              (Hashable)

import qualified Data.HashMap.Mutable.Basic as MHM

import qualified Data.HashMap.Strict        as HM

import           Flow                       ((.>))

--------------------------------------------------------------------------------

-- | FIXME: doc
insertWith
  :: (Eq k, Hashable k, PrimMonad m)
  => MHM.MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> v
  -- ^ FIXME: doc
  -> (v -> v -> m v)
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
insertWith hm k v combiner = do
  current <- MHM.lookup hm k
  value <- case current of
             Just v' -> combiner v v'
             Nothing -> pure v
  MHM.insert hm k value

foldM
  :: (Eq k, Hashable k, PrimMonad m)
  => MHM.MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> a
  -- ^ FIXME: doc
  -> (k -> v -> a -> m a)
  -- ^ FIXME: doc
  -> m a
  -- ^ FIXME: doc
foldM hm initial combine
  = MHM.foldM (\x k v -> combine k v x) initial hm

freeze
  :: (Eq k, Hashable k, PrimMonad m)
  => MHM.MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> m (HM.HashMap k v)
  -- ^ FIXME: doc
freeze hm = do
  let collision = error "MHashMap.freeze: collision occurred! this is a bug."
  foldM hm HM.empty
    $ \k v -> HM.insertWith (\_ _ -> collision) k v .> pure

thaw
  :: (Eq k, Hashable k, PrimMonad m)
  => HM.HashMap k v
  -- ^ FIXME: doc
  -> m (MHM.MHashMap (PrimState m) k v)
  -- ^ FIXME: doc
thaw hm = do
  let fold :: (Monad m) => HM.HashMap k v -> a -> (k -> v -> a -> m a) -> m a
      fold hmap initial combine
        = HM.foldrWithKey (\k v x -> x >>= combine k v) (pure initial) hmap
  result <- MHM.newSized (HM.size hm)
  fold hm () $ \k v () -> MHM.insert result k v
  pure result

--------------------------------------------------------------------------------
