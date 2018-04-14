--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module Matrix
  ( module Matrix
  ) where

--------------------------------------------------------------------------------

import           UnexceptionalIO                 (UIO)
import qualified UnexceptionalIO                 as UIO

import           Control.Arrow                   ((&&&))

import           Control.Exception               (SomeException)

import           Control.Monad

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

-- | This datatype is meant to be used with the @-XDataKinds@ language extension
--   to tag whether a matrix is meant to be _dense_ or _sparse_.
--
--   A _dense_ matrix is an in-memory representation of an @m × n@ matrix valued
--   in a semiring @R@ that uses @Θ(m · n · log₂(|R|))@ bits of storage.
--
--   A _sparse_ matrix is an in-memory representation of an @m × n@ matrix @M@
--   valued in a semiring @R@ that uses @Θ(nnz(M) · log₂(|R|))@ bits of storage,
--   where @nnz(M)@ is the number of nonzero elements of @M@.
data Packing
  = -- | A type-level tag for dense matrices.
    Dense
  | -- | A type-level tag for sparse matrices.
    Sparse
  deriving (Eq, Show, Read)

-- | FIXME: doc
data Mutability
  = -- | FIXME: doc
    Immutable
  | -- | FIXME: doc
    Mutable
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
type MatrixPos = (Int, Int) -- FIXME: use newtype instead?

-- | FIXME: doc
type MatrixShape = (Int, Int) -- FIXME: use newtype instead?

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

  -- | Get the shape of the given matrix.
  shapeMatrix
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix to get the shape of.
    -> MatrixShape
    -- ^ The shape of that matrix.

  -- | Add two matrices together.
  addMatrix
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ The left hand side of the addition.
    -> Matrix p a b
    -- ^ The right hand side of the addition.
    -> Matrix p a b
    -- ^ The sum of the two given matrices.

  -- | Subtract one matrix from another.
  subMatrix
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ The left hand side of the subtraction.
    -> Matrix p a b
    -- ^ The right hand side of the subtraction.
    -> Matrix p a b
    -- ^ The difference between the two given matrices.

  -- | Compute the product of two matrices.
  mulMatrix
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ The left hand side of the product.
    -> Matrix p a b
    -- ^ The right hand side of the product.
    -> Matrix p a b
    -- ^ The product of the two given matrices.

-- | FIXME: doc
instance IsMatrix 'Dense where
  zeroMatrix       = uncurry Eigen.Matrix.zero
  thawMatrix       = Eigen.Matrix.thaw
  unsafeThawMatrix = Eigen.Matrix.unsafeThaw
  shapeMatrix      = Eigen.Matrix.dims
  addMatrix        = (+)
  subMatrix        = (-)
  mulMatrix        = (*)

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
  shapeMatrix = Eigen.SparseMatrix.rows &&& Eigen.SparseMatrix.cols
  addMatrix = (+)
  subMatrix = (-)
  mulMatrix = (*)

--------------------------------------------------------------------------------

-- | FIXME: doc
class IsMutableMatrix (p :: Packing) where
  -- | Create a new 'MutableMatrix' with the given shape.
  newMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MatrixShape
    -- ^ The shape of the 'MutableMatrix' to create.
    -> m (MutableMatrix p a b (PrimState m))
    -- ^ A 'MutableMatrix' with the given shape, containing all zeros.
    --   FIXME: is that true? double check

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
    -- ^ A 'PrimMonad' action that reserves the space.

  -- | Returns a boolean representing whether the given 'MutableMatrix' is
  --   valid; for sparse matrices this is always true but for dense matrices
  --   it may return 'False'.
  validMutableMatrix
    :: (Eigen.Elem a b)
    => MutableMatrix p a b s
    -- ^ The 'MutableMatrix' to check the validity of.
    -> Bool
    -- ^ 'True' if the given matrix is valid, 'False' otherwise.

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
    -- ^ A 'PrimMonad' action returning the value of the element at the
    --   given position in the given matrix.

  -- | Like 'getMutableMatrix', but potentially with better performance, and
  --   bounds-checking is not guaranteed.
  unsafeGetMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ The matrix in which an element will be retrieved.
    -> MatrixPos
    -- ^ The position of the element to get.
    -> m a
    -- ^ A 'PrimMonad' action returning the value of the element at the
    --   given position in the given matrix.

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
    -- ^ A 'PrimMonad' action setting the element at the given position in the
    --   given matrix to the given value.

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
    -- ^ A 'PrimMonad' action setting the element at the given position in the
    --   given matrix to the given value.

  -- | Convert a 'MutableMatrix' to a 'Matrix' with the same packing.
  freezeMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ The 'MutableMatrix' to freeze.
    -> m (Matrix p a b)
    -- ^ An immutable 'Matrix'.

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
    -- ^ An immutable 'Matrix'.

  -- | Returns the shape of the given 'MutableMatrix'.
  shapeMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ A 'MutableMatrix' to find the shape of.
    -> m MatrixShape
    -- ^ A 'PrimMonad' action returning the shape of the given matrix.

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
  shapeMutableMatrix        = (Eigen.MMatrix.mm_rows &&& Eigen.MMatrix.mm_cols)
                              .> pure

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
  shapeMutableMatrix matrix = stToPrim $ do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    r <- unsafeIOToST (Eigen.IOSparseMatrix.rows m)
    -- FIXME: verify that this is safe
    c <- unsafeIOToST (Eigen.IOSparseMatrix.cols m)
    pure (r, c)

--------------------------------------------------------------------------------

-- | FIXME: doc
class ConvertMatrix (p1 :: Packing) (p2 :: Packing) where
  -- | Convert a 'Matrix' from one 'Packing' to another.
  --
  --   This function can be fruitfully used with @-XTypeApplications@ to
  --   concisely specify the source and target representations; for example:
  --
  --   @
  --     {-# LANGUAGE TypeApplications #-}
  --
  --     toSparse :: ('Eigen.Elem' a b)
  --              => 'Matrix' p a b -> 'Matrix' 'Sparse' a b
  --     toSparse = 'convertMatrix' @_ @'Sparse'
  --
  --     fromSparse :: ('Eigen.Elem' a b)
  --                => 'Matrix' 'Sparse' a b -> 'Matrix' p a b
  --     fromSparse = 'convertMatrix' @'Sparse' @_
  --   @
  convertMatrix
    :: (Eigen.Elem a b)
    => Matrix p1 a b
    -- ^ A matrix to convert the representation of.
    -> Matrix p2 a b
    -- ^ A new matrix with the new representation.

  -- | Convert a 'MutableMatrix' from one 'Packing' to another.
  --
  --   Like 'convertMatrix', this can be fruitfully used with
  --   @-XTypeApplications@ to concisely convert between representations.
  --
  --   Note that this function can be inefficient, as in the nontrivial case
  --   where @p1 ≠ p2@, it will freeze the mutable matrix (a copy), convert
  --   the frozen matrix (another copy), and then unsafely thaw the result
  --   (this is safe because we know that we just created this copy, so no
  --   other references to it can exist).
  convertMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p1 a b (PrimState m)
    -- ^ A mutable matrix to convert the representation of.
    -> m (MutableMatrix p2 a b (PrimState m))
    -- ^ A new mutable matrix with the new representation.

-- | Trivial; @'convertMatrix' = 'id'@ and @'convertMutableMatrix' = 'pure'@.
instance ConvertMatrix 'Dense 'Dense where
  convertMatrix = id
  convertMutableMatrix = pure

-- | Trivial; @'convertMatrix' = 'id'@ and @'convertMutableMatrix' = 'pure'@.
instance ConvertMatrix 'Sparse 'Sparse where
  convertMatrix = id
  convertMutableMatrix = pure

-- | Nontrivial; 'convertMatrix' uses 'Eigen.SparseMatrix.fromMatrix'
--   and 'convertMutableMatrix' delegates to 'convertMatrix' by freezing,
--   converting, and then thawing.
instance ConvertMatrix 'Dense 'Sparse where
  convertMatrix = Eigen.SparseMatrix.fromMatrix
  convertMutableMatrix = freezeMutableMatrix
                         >=> (convertMatrix .> pure)
                         >=> unsafeThawMatrix

-- | Nontrivial; 'convertMatrix' uses 'Eigen.SparseMatrix.toMatrix'
--   and 'convertMutableMatrix' delegates to 'convertMatrix' by freezing,
--   converting, and then thawing.
instance ConvertMatrix 'Sparse 'Dense where
  convertMatrix = Eigen.SparseMatrix.toMatrix
  convertMutableMatrix = freezeMutableMatrix
                         >=> (convertMatrix .> pure)
                         >=> unsafeThawMatrix

--------------------------------------------------------------------------------

-- | Checks whether the given immutable sparse matrix is in compressed mode.
isCompressedSparseMatrix
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -- ^ An immutable sparse matrix.
  -> Bool
  -- ^ 'True' if the matrix was in compressed mode, 'False' otherwise.
isCompressedSparseMatrix = Eigen.SparseMatrix.compressed

-- | Convert the given immutable sparse matrix to compressed mode.
compressSparseMatrix
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -- ^ An immutable sparse matrix that may not be in compressed mode.
  --
  --   FIXME: is it true that matrices in compressed mode are tolerated?
  -> Matrix 'Sparse a b
  -- ^ An immutable sparse matrix in compressed mode with the same data.
compressSparseMatrix = Eigen.SparseMatrix.compress

-- | Convert the given immutable sparse matrix to uncompressed mode.
uncompressSparseMatrix
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -- ^ An immutable sparse matrix that may be in compressed mode.
  --
  --   FIXME: is it true that matrices not in compressed mode are tolerated?
  -> Matrix 'Sparse a b
  -- ^ An immutable sparse matrix in uncompressed mode with the same data.
uncompressSparseMatrix = Eigen.SparseMatrix.uncompress

--------------------------------------------------------------------------------

-- | Checks whether the given mutable sparse matrix is in compressed mode.
isCompressedSparseMutableMatrix
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ A mutable sparse matrix.
  -> m Bool
  -- ^ A 'PrimMonad' action returning 'True' if the given matrix was in
  --   compressed mode and 'False' otherwise.
isCompressedSparseMutableMatrix matrix = stToPrim $ do
  let (MSparseMatrix m) = matrix
  unsafeIOToST (Eigen.IOSparseMatrix.compressed m)

-- | Convert the given mutable sparse matrix to compressed mode.
compressSparseMutableMatrix
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ A mutable sparse matrix that may be in compressed mode.
  --
  --   FIXME: is it true that matrices in compressed mode are tolerated?
  -> m ()
  -- ^ A 'PrimMonad' action that converts the matrix to compressed mode.
compressSparseMutableMatrix matrix = stToPrim $ do
  let (MSparseMatrix m) = matrix
  unsafeIOToST (Eigen.IOSparseMatrix.compress m)

-- | Convert the given mutable sparse matrix to uncompressed mode.
uncompressSparseMutableMatrix
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ A mutable sparse matrix that may be in compressed mode.
  --
  --   FIXME: is it true that matrices not in compressed mode are tolerated?
  -> m ()
  -- ^ A 'PrimMonad' action that converts the matrix to uncompressed mode.
uncompressSparseMutableMatrix matrix = stToPrim $ do
  let (MSparseMatrix m) = matrix
  unsafeIOToST (Eigen.IOSparseMatrix.uncompress m)

--------------------------------------------------------------------------------

-- Eigen.MMatrix.replicate
-- Eigen.IOSparseMatrix.innerSize
-- Eigen.IOSparseMatrix.outerSize
-- Eigen.IOSparseMatrix.nonZeros
-- Eigen.IOSparseMatrix.setZero
-- Eigen.IOSparseMatrix.setIdentity
-- Eigen.IOSparseMatrix.resize
-- Eigen.IOSparseMatrix.conservativeResize

--------------------------------------------------------------------------------
