--------------------------------------------------------------------------------

module MutableBitmap
  ( MutableBitmap
  , new, size, get, set, fill, isAllTrue, isAllFalse
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)

import qualified Foundation              as F
import qualified Foundation.Array        as F.Array
import qualified Foundation.Collection   as F.Collection

--------------------------------------------------------------------------------

data MutableBitmap s
  = UnsafeMkMutableBitmap
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !(F.Array.MutableBitmap s)
  deriving ()

--------------------------------------------------------------------------------

new :: (PrimMonad m) => Int -> m (MutableBitmap (PrimState m))
new n = stToPrim $ do
  bitmap <- F.Collection.mutNew (fromIntegral n)
  let result = UnsafeMkMutableBitmap n bitmap
  fill result False
  pure result

size :: MutableBitmap s -> Int
size (UnsafeMkMutableBitmap n _) = n

fill :: (PrimMonad m) => MutableBitmap (PrimState m) -> Bool -> m ()
fill bitmap b = stToPrim $ do
  let (UnsafeMkMutableBitmap n bm) = bitmap
  let go i = if i < 0
             then pure ()
             else F.Collection.mutUnsafeWrite bm (F.Offset i) b >> go (i - 1)
  go (n - 1)

get :: (PrimMonad m) => MutableBitmap (PrimState m) -> Int -> m Bool
get bitmap i = stToPrim $ do
  let (UnsafeMkMutableBitmap _ bm) = bitmap
  F.Collection.mutRead bm (F.Offset i)

set :: (PrimMonad m) => MutableBitmap (PrimState m) -> Int -> Bool -> m ()
set bitmap i b = stToPrim $ do
  let (UnsafeMkMutableBitmap _ bm) = bitmap
  F.Collection.mutWrite bm (F.Offset i) b

isAllTrue :: (PrimMonad m) => MutableBitmap (PrimState m) -> m Bool
isAllTrue bitmap = stToPrim $ do
  let (UnsafeMkMutableBitmap n bm) = bitmap
  let go i = if i < 0
             then pure True
             else do b <- F.Collection.mutUnsafeRead bm (F.Offset i)
                     (b &&) <$> go (i - 1)
  go (n - 1)

isAllFalse :: (PrimMonad m) => MutableBitmap (PrimState m) -> m Bool
isAllFalse bitmap = stToPrim $ do
  let (UnsafeMkMutableBitmap n bm) = bitmap
  let go i = if i < 0
             then pure True
             else do b <- F.Collection.mutUnsafeRead bm (F.Offset i)
                     (b ||) <$> go (i - 1)
  not <$> go (n - 1)

--------------------------------------------------------------------------------
