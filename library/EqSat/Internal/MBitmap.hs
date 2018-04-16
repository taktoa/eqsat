--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Internal.MBitmap
  ( MBitmap
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
data MBitmap s
  = UnsafeMkMBitmap
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !(F.Array.MutableBitmap s)
  deriving ()

--------------------------------------------------------------------------------

-- | FIXME: doc
new
  :: (PrimMonad m)
  => Int
  -- ^ FIXME: doc
  -> m (MBitmap (PrimState m))
  -- ^ FIXME: doc
new n = stToPrim $ do
  bitmap <- F.Collection.mutNew (fromIntegral n)
  let result = UnsafeMkMBitmap n bitmap
  fill result False
  pure result

-- | FIXME: doc
size
  :: MBitmap s
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
size (UnsafeMkMBitmap n _) = n

-- | FIXME: doc
fill
  :: (PrimMonad m)
  => MBitmap (PrimState m)
  -- ^ FIXME: doc
  -> Bool
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
fill bitmap b = stToPrim $ do
  let (UnsafeMkMBitmap n bm) = bitmap
  let go i = if i < 0
             then pure ()
             else F.Collection.mutUnsafeWrite bm (F.Offset i) b >> go (i - 1)
  go (n - 1)

-- | FIXME: doc
get
  :: (PrimMonad m)
  => MBitmap (PrimState m)
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
get bitmap i = stToPrim $ do
  let (UnsafeMkMBitmap _ bm) = bitmap
  F.Collection.mutRead bm (F.Offset i)

-- | FIXME: doc
set
  :: (PrimMonad m)
  => MBitmap (PrimState m)
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
  -> Bool
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
set bitmap i b = stToPrim $ do
  let (UnsafeMkMBitmap _ bm) = bitmap
  F.Collection.mutWrite bm (F.Offset i) b

-- | FIXME: doc
isAllTrue
  :: (PrimMonad m)
  => MBitmap (PrimState m)
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
isAllTrue bitmap = stToPrim $ do
  let (UnsafeMkMBitmap n bm) = bitmap
  let go i = if i < 0
             then pure True
             else do b <- F.Collection.mutUnsafeRead bm (F.Offset i)
                     (b &&) <$> go (i - 1)
  go (n - 1)

-- | FIXME: doc
isAllFalse
  :: (PrimMonad m)
  => MBitmap (PrimState m)
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
isAllFalse bitmap = stToPrim $ do
  let (UnsafeMkMBitmap n bm) = bitmap
  let go i = if i < 0
             then pure True
             else do b <- F.Collection.mutUnsafeRead bm (F.Offset i)
                     (b ||) <$> go (i - 1)
  not <$> go (n - 1)

--------------------------------------------------------------------------------
