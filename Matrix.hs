--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}

--------------------------------------------------------------------------------

module Matrix
  ( module Matrix
  ) where

import           UnexceptionalIO                 (UIO)
import qualified UnexceptionalIO                 as UIO

import           Control.Exception               (SomeException)

import           Control.Monad.Primitive

import           Control.Monad.Trans.Except      (ExceptT (ExceptT))
import qualified Control.Monad.Trans.Except      as ExceptT

import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Trans.Class       (MonadTrans (lift))

import           Control.Monad.ST.Unsafe         (unsafeIOToST)

import           Data.Kind                       (Type)

import           Data.Coerce                     (coerce)

import qualified Data.Eigen.Matrix               as Eigen (Matrix)
import qualified Data.Eigen.Matrix.Mutable       as Eigen (MMatrix)
import qualified Data.Eigen.SparseMatrix         as Eigen (SparseMatrix)
import qualified Data.Eigen.SparseMatrix.Mutable as Eigen (IOSparseMatrix)

import qualified Data.Eigen.Matrix               as Eigen (Elem)

import qualified Data.Eigen.LA                   as Eigen.LA
import qualified Data.Eigen.Matrix               as Eigen.Matrix
import qualified Data.Eigen.Matrix.Mutable       as Eigen.MMatrix
import qualified Data.Eigen.Parallel             as Eigen.Parallel
import qualified Data.Eigen.SparseLA             as Eigen.SparseLA
import qualified Data.Eigen.SparseMatrix         as Eigen.SparseMatrix
import qualified Data.Eigen.SparseMatrix.Mutable as Eigen.IOSparseMatrix

import           Data.Proxy                      (Proxy (Proxy))

import           Flow                            ((.>), (|>))

--------------------------------------------------------------------------------

-- | FIXME: doc
type Eigen a = ExceptT SomeException UIO a

-- | FIXME: doc
ioToEigen :: IO a -> Eigen a
ioToEigen = UIO.fromIO .> ExceptT

--------------------------------------------------------------------------------

-- | FIXME: doc
setEigenThreads :: Int -> Eigen ()
setEigenThreads = Eigen.Parallel.setNbThreads .> ioToEigen

-- | FIXME: doc
getEigenThreads :: Eigen Int
getEigenThreads = Eigen.Parallel.getNbThreads |> ioToEigen

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype MSparseMatrix a b s
  = MSparseMatrix (Eigen.IOSparseMatrix a b)

--------------------------------------------------------------------------------

-- | FIXME: doc
data Packing
  = -- | FIXME: doc
    Dense
  | -- | FIXME: doc
    Sparse
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------

-- | FIXME: doc
type family Matrix (p :: Packing) = mat | mat -> p where
  Matrix 'Dense  = Eigen.Matrix
  Matrix 'Sparse = Eigen.SparseMatrix

-- | FIXME: doc
type family MutableMatrix (p :: Packing) = mat | mat -> p where
  MutableMatrix 'Dense  = Eigen.MMatrix
  MutableMatrix 'Sparse = MSparseMatrix

--------------------------------------------------------------------------------

-- | FIXME: doc
type MatrixPos   = (Int, Int)

-- | FIXME: doc
type MatrixShape = (Int, Int)

--------------------------------------------------------------------------------

-- | FIXME: doc
class IsMatrix (p :: Packing) where
  -- | Create a new 'Matrix' filled with zeroes
  --   (as defined by the 'Elem' instance).
  zeroMatrix
    :: (Eigen.Elem a b)
    => MatrixShape
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc

  -- | Convert a 'Matrix' to a 'MutableMatrix' with the same packing.
  thawMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => Matrix p a b
    -- ^ FIXME: doc
    -> m (MutableMatrix p a b (PrimState m))
    -- ^ FIXME: doc

  -- | Like 'thawMatrix', except the 'Matrix' given to this function can no
  --   longer be used, and the performance may be better.
  --
  --   If Haskell had uniqueness types, this function would be safe, as the
  --   function could take a unique 'Matrix', guaranteeing that there are no
  --   copies lingering around to break referential transparency.
  unsafeThawMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => Matrix p a b
    -- ^ FIXME: doc
    -> m (MutableMatrix p a b (PrimState m))
    -- ^ FIXME: doc

-- | FIXME: doc
instance IsMatrix 'Dense where
  zeroMatrix       = uncurry Eigen.Matrix.zero
  thawMatrix       = Eigen.Matrix.thaw
  unsafeThawMatrix = Eigen.Matrix.unsafeThaw

-- | FIXME: doc
instance IsMatrix 'Sparse where
  zeroMatrix (x, y) = Eigen.SparseMatrix.fromList x y []
  thawMatrix matrix = stToPrim $ do
    -- FIXME: verify that this is safe
    m <- unsafeIOToST (Eigen.SparseMatrix.thaw matrix)
    pure (MSparseMatrix m)
  unsafeThawMatrix matrix = stToPrim $ do
    -- FIXME: verify that this is safe
    m <- unsafeIOToST (Eigen.SparseMatrix.unsafeThaw matrix)
    pure (MSparseMatrix m)

--------------------------------------------------------------------------------

-- | FIXME: doc
class IsMutableMatrix (p :: Packing) where
  -- | Create a new 'MutableMatrix' with the given shape.
  newMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MatrixShape
    -- ^ The shape of the 'MutableMatrix' to create.
    -> m (MutableMatrix p a b (PrimState m))
    -- ^ FIXME: doc

  -- | Preallocate space for nonzero elements in the given 'MutableMatrix'.
  --
  --   This does nothing for dense matrices, but for sparse matrices it checks
  --   that the matrix is in compressed mode, compresses it if it is not, and
  --   then preallocates the given number of elements.
  reserveMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => Int
    -- ^ The number of nonzero elements you want to preallocate.
    -> MutableMatrix p a b (PrimState m)
    -- ^ The mutable matrix in which the space will be reserved.
    -> m ()
    -- ^ FIXME: doc

  -- | Returns a boolean representing whether the given 'MutableMatrix' is
  --   valid; for sparse matrices this is always true but for dense matrices
  --   it may return 'False'.
  validMutableMatrix
    :: (Eigen.Elem a b)
    => MutableMatrix p a b s
    -- ^ The 'MutableMatrix' to check the validity of.
    -> Bool
    -- ^ FIXME: doc

  -- | Get the element of the given matrix at the given position.
  --
  --   This function will throw an exception if the position is out of bounds.
  getMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ The matrix in which an element will be retrieved.
    -> MatrixPos
    -- ^ The position of the element to get.
    -> m a
    -- ^ FIXME: doc

  -- | Like 'getMutableMatrix', but potentially with better performance, and
  --   bounds-checking is not guaranteed.
  unsafeGetMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ The matrix in which an element will be retrieved.
    -> MatrixPos
    -- ^ The position of the element to get.
    -> m a
    -- ^ FIXME: doc

  -- | Set the element of the given matrix at the given position to the given
  --   value.
  --
  --   This function will throw an exception if the position is out of bounds.
  setMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ The matrix to modify.
    -> MatrixPos
    -- ^ The position in the matrix that will be modified.
    -> a
    -- ^ The new value that that position will be set to.
    -> m ()
    -- ^ FIXME: doc

  -- | Like 'setMutableMatrix', but potentially with better performance, and
  --   bounds-checking is not guaranteed.
  unsafeSetMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ The matrix to modify.
    -> MatrixPos
    -- ^ The position in the matrix that will be modified.
    -> a
    -- ^ The new value that that position will be set to.
    -> m ()
    -- ^ FIXME: doc

  -- | Convert a 'MutableMatrix' to a 'Matrix' with the same packing.
  freezeMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ The 'MutableMatrix' to freeze.
    -> m (Matrix p a b)
    -- ^ FIXME: doc

  -- | Like 'freezeMutableMatrix', except the 'MutableMatrix' given to this
  --   function can no longer be used, and the performance may be better.
  --
  --   If Haskell had uniqueness types, this function would be safe, as the
  --   function could take a unique 'MutableMatrix', guaranteeing that there
  --   are no copies lingering around to break referential transparency.
  unsafeFreezeMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ The 'MutableMatrix' to freeze.
    -> m (Matrix p a b)
    -- ^ FIXME: doc

-- | FIXME: doc
instance IsMutableMatrix 'Dense where
  newMutableMatrix          = uncurry Eigen.MMatrix.new
  reserveMutableMatrix _ _  = pure ()
  validMutableMatrix        = Eigen.MMatrix.valid
  getMutableMatrix          = Eigen.MMatrix.read  .> uncurry
  setMutableMatrix          = Eigen.MMatrix.write .> uncurry
  unsafeGetMutableMatrix    = Eigen.MMatrix.unsafeRead  .> uncurry
  unsafeSetMutableMatrix    = Eigen.MMatrix.unsafeWrite .> uncurry
  freezeMutableMatrix       = Eigen.Matrix.freeze
  unsafeFreezeMutableMatrix = Eigen.Matrix.unsafeFreeze

-- | FIXME: doc
instance IsMutableMatrix 'Sparse where
  newMutableMatrix (x, y) = stToPrim $ do
    -- FIXME: verify that this is safe
    m <- unsafeIOToST (Eigen.IOSparseMatrix.new x y)
    pure (MSparseMatrix m)
  reserveMutableMatrix space matrix = stToPrim $ do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    unsafeIOToST (Eigen.IOSparseMatrix.reserve m space)
  validMutableMatrix = const True
  getMutableMatrix matrix (x, y) = stToPrim $ do
    -- FIXME: verify that this is safe
    unsafeIOToST (Eigen.IOSparseMatrix.read (coerce matrix) x y)
  setMutableMatrix matrix (x, y) el = stToPrim $ do
    -- FIXME: verify that this is safe
    unsafeIOToST (Eigen.IOSparseMatrix.write (coerce matrix) x y el)
  unsafeGetMutableMatrix = getMutableMatrix
  unsafeSetMutableMatrix = setMutableMatrix
  freezeMutableMatrix matrix = stToPrim $ do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    unsafeIOToST (Eigen.SparseMatrix.freeze m)
  unsafeFreezeMutableMatrix matrix = stToPrim $ do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    unsafeIOToST (Eigen.SparseMatrix.unsafeFreeze m)

--------------------------------------------------------------------------------

-- | FIXME: doc
isCompressedMatrix
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -- ^ FIXME: doc
  -> Bool
  -- ^ FIXME: doc
isCompressedMatrix = Eigen.SparseMatrix.compressed

-- | FIXME: doc
compressMatrix
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -- ^ FIXME: doc
  -> Matrix 'Sparse a b
  -- ^ FIXME: doc
compressMatrix = Eigen.SparseMatrix.compress

-- | FIXME: doc
uncompressMatrix
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -- ^ FIXME: doc
  -> Matrix 'Sparse a b
  -- ^ FIXME: doc
uncompressMatrix = Eigen.SparseMatrix.uncompress

-- | FIXME: doc
isCompressedMutableMatrix
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
isCompressedMutableMatrix matrix = stToPrim $ do
  let (MSparseMatrix m) = matrix
  unsafeIOToST (Eigen.IOSparseMatrix.compressed m)

-- | FIXME: doc
compressMutableMatrix
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
compressMutableMatrix matrix = stToPrim $ do
  let (MSparseMatrix m) = matrix
  unsafeIOToST (Eigen.IOSparseMatrix.compress m)

-- | FIXME: doc
uncompressMutableMatrix
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
uncompressMutableMatrix matrix = stToPrim $ do
  let (MSparseMatrix m) = matrix
  unsafeIOToST (Eigen.IOSparseMatrix.uncompress m)

--------------------------------------------------------------------------------

-- Eigen.MMatrix.replicate
-- Eigen.IOSparseMatrix.setZero
-- Eigen.IOSparseMatrix.setIdentity

--------------------------------------------------------------------------------
