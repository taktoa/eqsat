--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.MHashMap
  ( MHashMap
  , new
  , newSized
  , delete
  , lookup
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
                 (Double, Eq, Int, Show, error, flip, ($))

import           Control.Applicative        (pure)
import           Control.Monad              (Monad ((>>=)))
import           Data.Functor               ((<$>))

import           Control.Monad.Primitive    (PrimMonad (PrimState))

import           Data.Maybe                 (Maybe (Just, Nothing))

import           Data.Hashable              (Hashable)

import qualified Data.HashMap.Mutable.Basic as MHM

import qualified Data.HashMap.Strict        as HM

import           Flow                       ((.>))

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype MHashMap s k v
  = MkMHashMap (MHM.MHashMap s k v)
  deriving (Show)

-- | FIXME: doc
new
  :: (PrimMonad m)
  => m (MHashMap (PrimState m) k v)
  -- ^ FIXME: doc
new = MkMHashMap <$> MHM.new

-- | FIXME: doc
newSized
  :: (PrimMonad m)
  => Int
  -- ^ FIXME: doc
  -> m (MHashMap (PrimState m) k v)
  -- ^ FIXME: doc
newSized size = MkMHashMap <$> MHM.newSized size

-- | FIXME: doc
delete
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
delete (MkMHashMap hm) = MHM.delete hm

-- | FIXME: doc
lookup
  :: (Eq k, Hashable k, PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> k
  -- ^ FIXME: doc
  -> m (Maybe v)
  -- ^ FIXME: doc
lookup (MkMHashMap hm) = MHM.lookup hm

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
insert (MkMHashMap hm) = MHM.insert hm

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
insertWith hm k v combiner = do
  value <- lookup hm k
           >>= (\case (Just v') -> combiner v v'
                      Nothing   -> pure v)
  insert hm k value

-- | FIXME: doc
mapM_
  :: (PrimMonad m)
  => (k -> v -> m any)
  -- ^ FIXME: doc
  -> MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
mapM_ f (MkMHashMap hm) = MHM.mapM_ f hm

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
foldM (MkMHashMap hm) initial combine
  = MHM.foldM (\x k v -> combine k v x) initial hm

-- | FIXME: doc
computeOverhead
  :: (PrimMonad m)
  => MHashMap (PrimState m) k v
  -- ^ FIXME: doc
  -> m Double
  -- ^ FIXME: doc
computeOverhead (MkMHashMap hm) = MHM.computeOverhead hm

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
  result <- newSized (HM.size hm)
  fold hm () $ \k v () -> insert result k v
  pure result

--------------------------------------------------------------------------------
