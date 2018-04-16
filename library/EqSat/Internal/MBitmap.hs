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
  , freezeToUVector
  , thawFromUVector
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.Primitive     (PrimMonad (PrimState), stToPrim)

import qualified Foundation                  as F
import qualified Foundation.Array            as F.Array
import qualified Foundation.Collection       as F.Collection

import qualified Data.Vector.Unboxed         as UVector
import qualified Data.Vector.Unboxed.Mutable as UMVector

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

-- | FIXME: doc
freezeToUVector
  :: (PrimMonad m)
  => MBitmap (PrimState m)
  -- ^ FIXME: doc
  -> m (UVector.Vector Bool)
  -- ^ FIXME: doc
freezeToUVector bitmap = stToPrim $ do
  let (UnsafeMkMBitmap n bm) = bitmap
  result <- UMVector.unsafeNew n
  let go i = if i < 0
             then pure ()
             else do b <- F.Collection.mutUnsafeRead bm (F.Offset i)
                     UMVector.unsafeWrite result i b
                     go (i - 1)
  go (n - 1)
  UVector.unsafeFreeze result

-- | FIXME: doc
thawFromUVector
  :: (PrimMonad m)
  => UVector.Vector Bool
  -- ^ FIXME: doc
  -> m (MBitmap (PrimState m))
  -- ^ FIXME: doc
thawFromUVector vector = stToPrim $ do
  let n = UVector.length vector
  bm <- F.Collection.mutNew (fromIntegral n)
  let go i = if i < 0
             then pure ()
             else do b <- UVector.unsafeIndexM vector i
                     F.Collection.mutUnsafeWrite bm (F.Offset i) b
                     go (i - 1)
  go (n - 1)
  pure $ UnsafeMkMBitmap n bm

--------------------------------------------------------------------------------
