--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.MutableBitmap
  ( MutableBitmap
  , new
  , size
  , get
  , set
  , fill
  , isAllTrue
  , isAllFalse
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.Primitive (PrimMonad (PrimState), stToPrim)

import qualified Foundation              as F
import qualified Foundation.Array        as F.Array
import qualified Foundation.Collection   as F.Collection

--------------------------------------------------------------------------------

-- | FIXME: doc
data MutableBitmap s
  = UnsafeMkMutableBitmap
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !(F.Array.MutableBitmap s)
  deriving ()

--------------------------------------------------------------------------------

-- | FIXME: doc
new
  :: (PrimMonad m)
  => Int
  -- ^ FIXME: doc
  -> m (MutableBitmap (PrimState m))
  -- ^ FIXME: doc
new n = stToPrim $ do
  bitmap <- F.Collection.mutNew (fromIntegral n)
  let result = UnsafeMkMutableBitmap n bitmap
  fill result False
  pure result

-- | FIXME: doc
size
  :: MutableBitmap s
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
size (UnsafeMkMutableBitmap n _) = n

-- | FIXME: doc
fill
  :: (PrimMonad m)
  => MutableBitmap (PrimState m)
  -- ^ FIXME: doc
  -> Bool
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
fill bitmap b = stToPrim $ do
  let (UnsafeMkMutableBitmap n bm) = bitmap
  let go i = if i < 0
             then pure ()
             else F.Collection.mutUnsafeWrite bm (F.Offset i) b >> go (i - 1)
  go (n - 1)

-- | FIXME: doc
get
  :: (PrimMonad m)
  => MutableBitmap (PrimState m)
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
get bitmap i = stToPrim $ do
  let (UnsafeMkMutableBitmap _ bm) = bitmap
  F.Collection.mutRead bm (F.Offset i)

-- | FIXME: doc
set
  :: (PrimMonad m)
  => MutableBitmap (PrimState m)
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
  -> Bool
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
set bitmap i b = stToPrim $ do
  let (UnsafeMkMutableBitmap _ bm) = bitmap
  F.Collection.mutWrite bm (F.Offset i) b

-- | FIXME: doc
isAllTrue
  :: (PrimMonad m)
  => MutableBitmap (PrimState m)
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
isAllTrue bitmap = stToPrim $ do
  let (UnsafeMkMutableBitmap n bm) = bitmap
  let go i = if i < 0
             then pure True
             else do b <- F.Collection.mutUnsafeRead bm (F.Offset i)
                     (b &&) <$> go (i - 1)
  go (n - 1)

-- | FIXME: doc
isAllFalse
  :: (PrimMonad m)
  => MutableBitmap (PrimState m)
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
isAllFalse bitmap = stToPrim $ do
  let (UnsafeMkMutableBitmap n bm) = bitmap
  let go i = if i < 0
             then pure True
             else do b <- F.Collection.mutUnsafeRead bm (F.Offset i)
                     (b ||) <$> go (i - 1)
  not <$> go (n - 1)

--------------------------------------------------------------------------------
