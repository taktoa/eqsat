--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE UnboxedTuples         #-}

--------------------------------------------------------------------------------

-- | A mutable stack implementation, parameterized by a type of underlying
--   vector from the @vector@ package.
module EqSat.Internal.MStack
  ( -- * 'MStack'
    MStack

    -- * Type aliases
  , UMStack, PMStack, SMStack, BMStack

    -- * Creation
  , new
  , newWithCapacity

    -- * Pushing, peeking, and popping
  , push
  , peek
  , pop
  , popMaybe

    -- * Freezing and thawing
  , thaw
  , freeze
  ) where

--------------------------------------------------------------------------------

import           Control.Monad

import           Control.Monad.Primitive
                 (PrimMonad (PrimState, primitive), primitive_)

import           Data.Vector                 (Vector)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GMV
import           Data.Vector.Mutable         (MVector)

import           Data.Vector.Primitive       (Prim)
import           Data.Vector.Storable        (Storable)
import           Data.Vector.Unboxed         (Unbox)

import qualified Data.Vector.Primitive       as PV
import qualified Data.Vector.Storable        as SV
import qualified Data.Vector.Unboxed         as UV

import qualified GHC.Prim
import qualified GHC.Types

import           Data.Bits                   (testBit, unsafeShiftR)
import           Data.Word                   (Word32)

import           Flow                        ((|>))

--------------------------------------------------------------------------------

-- FIXME: these should be Word32# when that is added to `ghc-prim`

-- FIXME: the mutable vector already has length information, albeit boxed, so
--        maybe we don't need the capacity MutVar#

-- FIXME: ideally we could replace this with something that reduces the number
--        of indirections (a MutVar# cannot contain an unboxed type) by using
--        a MutableByteArray# and a Storable or Unbox instance, though this
--        will require reimplementing a decent amount of functionality from
--        the `vector` package.

-- | A mutable stack, parameterized on the type of (immutable) vector you want
--   to use. Generally these will be, in order from most preferable to least:
--
--   * @Data.Vector.Unboxed.'UV.Vector'@ for values with an 'Unbox' instance.
--     This is a 'UMStack'.
--   * @Data.Vector.Primitive.'PV.Vector'@ for values with a 'Prim' instance.
--     This is a 'PMStack'.
--   * @Data.Vector.Storable.'SV.Vector'@ for values with a 'Storable' instance.
--     This is an 'SMStack'.
--   * @Data.Vector.'Vector'@ for any boxed value.
--     This is a 'BMStack' (@B@ for "boxed").
--
--   Each 'MStack' you create adds three entries to the garbage collector's
--   table of mutable references, which it must traverse in linear time every
--   time a GC is performed, so you should not create large data structures
--   containing many 'MStack's.
--
--   It may be worth keeping in mind that the amount of space allocated for the
--   'MStack' is never less than about 64 bytes (the underlying vector always
--   has 8 elements).
data MStack vec s a
  = UnsafeMkMStack
    { _MStack_length
      :: !(GHC.Prim.MutVar# s Int)
      -- ^ A 'GHC.Prim.MutVar#' containing the length of the stack.
    , _MStack_capacity
      :: !(GHC.Prim.MutVar# s Int)
      -- ^ A 'GHC.Prim.MutVar#' containing the size of the underlying vector.
    , _MStack_vector
      :: !(GHC.Prim.MutVar# s (GV.Mutable vec s a))
      -- ^ We use a 'GHC.Prim.MutVar#' over the mutable vector so that we can
      --   allow shrinking.
    }

--------------------------------------------------------------------------------

-- | An 'MStack' whose element type is an instance of 'Unbox'.
type UMStack s a = MStack UV.Vector s a

-- | An 'MStack' whose element type is an instance of 'Prim'.
type PMStack s a = MStack PV.Vector s a

-- | An 'MStack' whose element type is an instance of 'Storable'.
type SMStack s a = MStack SV.Vector s a

-- | An 'MStack' whose element type is any boxed value.
type BMStack s a = MStack Vector s a

--------------------------------------------------------------------------------

-- * Creation

-- | This is equivalent to @'newWithCapacity' 8@, and will get inlined.
--
--   I chose 8 as a good default because a pointer is usually 8 bytes and
--   a cache line is usually 64 bytes. I thought about making this function
--   detect the cache line size and pointer size, but I'd either have to use
--   Template Haskell (and assume that the build system is the same as the
--   system on which it runs) or I'd be making this function unnecessarily slow.
--
--   The default number should not be too large, since you may not end up
--   growing the stack that large (and it takes time to allocate all that
--   memory), but shouldn't be too small either, since you need to
--   reallocate every time the stack grows beyond its capacity.
new
  :: (GV.Vector vec a, PrimMonad m)
  => m (MStack vec (PrimState m) a)
  -- ^ A new stack with capacity 8 and length 0.
new = newWithCapacity 8
{-# INLINE new #-}

-- | Create a new 'MStack' with the given initial capacity.
newWithCapacity
  :: (GV.Vector vec a, PrimMonad m)
  => Word32
  -- ^ The desired initial capacity for the 'MStack'.
  -> m (MStack vec (PrimState m) a)
  -- ^ A new stack with the given capacity, rounded up to the nearest even
  --   number.
newWithCapacity cap = do
  let capEven :: Int
      capEven = (if testBit cap 0 -- if LSB is 1, then `cap` is odd
                 then fromIntegral (cap + 1)
                 else fromIntegral cap)
                |> (\x -> if x < 8 then 8 else x)
  vec <- GMV.unsafeNew capEven
  primitive $ \s0 -> do
    let (# s1, lenRef #) = GHC.Prim.newMutVar# 0       s0
    let (# s2, capRef #) = GHC.Prim.newMutVar# capEven s1
    let (# s3, vecRef #) = GHC.Prim.newMutVar# vec     s2
    (# s3, UnsafeMkMStack lenRef capRef vecRef #)
{-# INLINE newWithCapacity #-}

--------------------------------------------------------------------------------

-- * Push, peek, and pop

-- | Push the given element onto the given 'MStack'.
push
  :: (GV.Vector vec a, PrimMonad m)
  => MStack vec (PrimState m) a
  -- ^ A stack onto which an element will be pushed.
  -> a
  -- ^ The element to push.
  -> m ()
  -- ^ A 'PrimMonad' action that pushes the element onto the stack.
push stack value = do
  let !(UnsafeMkMStack lenRef capRef vecRef) = stack
  len <- primitive (GHC.Prim.readMutVar# lenRef)
  cap <- primitive (GHC.Prim.readMutVar# capRef)
  if | ((len + 1) >= cap) -> do
         primitive (GHC.Prim.atomicModifyMutVar# capRef (* 2))
         oldVec <- primitive (GHC.Prim.readMutVar# vecRef)
         newVec <- GMV.unsafeGrow oldVec cap
         primitive_ (GHC.Prim.writeMutVar# vecRef newVec)
         primitive (GHC.Prim.atomicModifyMutVar# lenRef (+ 1))
         GMV.unsafeWrite newVec len value
     | otherwise -> do
         primitive (GHC.Prim.atomicModifyMutVar# lenRef (+ 1))
         vec <- primitive (GHC.Prim.readMutVar# vecRef)
         GMV.unsafeWrite vec len value
  pure ()
{-# INLINABLE push #-}

-- FIXME: maybe offer a version that returns Maybe# (https://git.io/vpCEW)

-- | Peek at the top element of the given 'MStack'.
--   If the 'MStack' is empty, then the given default value is returned.
--   Otherwise, the return value is the result of running the given function
--   on the top element.
peek
  :: (GV.Vector vec a, PrimMonad m)
  => MStack vec (PrimState m) a
  -- ^ The stack off of which an element will be popped.
  -> b
  -- ^ A default value to return if the stack is empty.
  -> (a -> b)
  -- ^ A function to run on the value if the stack is not empty.
  -> m b
  -- ^ The result of peeking at the top element of the stack.
peek stack def f = do
  let !(UnsafeMkMStack lenRef capRef vecRef) = stack
  len <- primitive (GHC.Prim.readMutVar# lenRef)
  cap <- primitive (GHC.Prim.readMutVar# capRef)
  if | (len == 0) -> do
         pure def
     | (len >= cap) -> do
         fail "peek: invariant violated: len >= cap"
     | (len < 0) -> do
         fail "peek: invariant violated: len is negative"
     | (cap < 8) -> do
         fail "peek: invariant violated: cap < 8"
     | (testBit cap 0) -> do
         fail "peek: invariant violated: cap is not even"
     | otherwise -> do
         vec <- primitive (GHC.Prim.readMutVar# vecRef)
         f <$> GMV.unsafeRead vec (len - 1)
{-# INLINABLE peek #-}

-- | Pop off the top element of the given 'MStack'.
--   If the 'MStack' is empty, then the given default value is returned.
--   Otherwise, the return value is the result of running the given function
--   on the top element.
pop
  :: (GV.Vector vec a, PrimMonad m)
  => MStack vec (PrimState m) a
  -- ^ The stack off of which an element will be popped.
  -> b
  -- ^ A default value to return if the stack is empty.
  -> (a -> b)
  -- ^ A function to run on the value if the stack is not empty.
  -> m b
  -- ^ The result of popping the top element off the stack.
pop stack def f = do
  val <- peek stack def f
  let !(UnsafeMkMStack lenRef capRef _) = stack
  len <- primitive (GHC.Prim.readMutVar# lenRef)
  cap <- primitive (GHC.Prim.readMutVar# capRef)
  when (((len + len) < cap) && (cap > 8)) $ do
    let newCap = cap `unsafeShiftR` 1 -- divide by 2
    unsafeCompact stack newCap
  when (len > 0) $ do
    primitive (GHC.Prim.atomicModifyMutVar# lenRef (subtract 1))
  pure val
{-# INLINABLE pop #-}

-- | Equivalent to @\\s → 'pop' s 'Nothing' 'Just'@.
popMaybe
  :: (GV.Vector vec a, PrimMonad m)
  => MStack vec (PrimState m) a
  -- ^ The stack off of which the top element will be popped.
  -> m (Maybe a)
  -- ^ 'Nothing' if the given stack is empty, or 'Just' the top element if it
  --   had at least one element.
popMaybe stack = pop stack Nothing Just
{-# INLINE popMaybe #-}

--------------------------------------------------------------------------------

-- | Convert an immutable vector from the @vector@ package to an 'MStack'
--   containing the same values.
thaw
  :: (GV.Vector vec a, PrimMonad m)
  => vec a
  -- ^ An immutable vector.
  -> m (MStack vec (PrimState m) a)
  -- ^ A 'PrimMonad' action returning a mutable stack containing the same
  --   values as the given vector.
thaw vector = do
  let len = GV.length vector
  vec <- GV.thaw vector
         >>= (\mv -> GMV.unsafeGrow mv len)
  let cap = len * 2
  primitive $ \s0 -> do
    let (# s1, lenRef #) = GHC.Prim.newMutVar# len s0
    let (# s2, capRef #) = GHC.Prim.newMutVar# cap s1
    let (# s3, vecRef #) = GHC.Prim.newMutVar# vec s2
    (# s3, UnsafeMkMStack lenRef capRef vecRef #)
{-# INLINABLE thaw #-}

-- | Convert an 'MStack' to an immutable vector from the @vector@ package
--   containing the same values.
--
--   This function does not mutate the given 'MStack'.
freeze
  :: (GV.Vector vec a, PrimMonad m)
  => MStack vec (PrimState m) a
  -- ^ A mutable stack.
  -> m (vec a)
  -- ^ A 'PrimMonad' action returning an immutable vector containing the same
  --   values as the given stack.
freeze stack = do
  let !(UnsafeMkMStack lenRef _ vecRef) = stack
  len <- primitive (GHC.Prim.readMutVar# lenRef)
  vec <- primitive (GHC.Prim.readMutVar# vecRef)
  GV.freeze (GMV.take len vec)
{-# INLINABLE freeze #-}

--------------------------------------------------------------------------------

-- Private helper functions

unsafeCompact
  :: (GV.Vector vec a, PrimMonad m)
  => MStack vec (PrimState m) a
  -> Int
  -> m ()
unsafeCompact stack newCap = do
  let !(UnsafeMkMStack _ capRef vecRef) = stack
  primitive_ (GHC.Prim.writeMutVar# capRef newCap)
  oldVec <- GMV.unsafeTake newCap <$> primitive (GHC.Prim.readMutVar# vecRef)
  newVec <- GMV.unsafeNew  newCap
  GMV.unsafeCopy oldVec newVec
  primitive_ (GHC.Prim.writeMutVar# vecRef newVec)
{-# INLINE unsafeCompact #-}

--------------------------------------------------------------------------------
