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

import           Control.Monad.Trans.Maybe       (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Maybe       as MaybeT

import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Trans.Class       (MonadTrans (lift))

import           Control.Monad.ST.Unsafe         (unsafeIOToST)

import qualified Data.ByteString.Lazy            as LBS

import           Data.Kind                       (Type)

import           Data.Coerce                     (coerce)

import           Data.Maybe                      (fromMaybe, mapMaybe)

import           Data.STRef                      (STRef)
import qualified Data.STRef                      as STRef

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
  = MSparseMatrix
    { fromMSparseMatrix :: Eigen.IOSparseMatrix a b }
  deriving ()

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

-- | This is used to document that a function consumes its argument (so it must
--   be a unique value).
type Unique a = a

--------------------------------------------------------------------------------

-- | FIXME: doc
class IsMatrix (p :: Packing) where
  -- | Create a new 'Matrix' filled with zeroes
  --   (as defined by the 'Elem' instance).
  zeroMatrix
    :: (Eigen.Elem a b)
    => MatrixShape
    -- ^ FIXME: doc
    -> Unique (Matrix p a b)
    -- ^ FIXME: doc

  -- | Convert a 'Matrix' to a 'MutableMatrix' with the same packing.
  thawMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => Matrix p a b
    -- ^ An immutable matrix to thaw.
    -> m (Unique (MutableMatrix p a b (PrimState m)))
    -- ^ A 'PrimMonad' action returning the thawed (mutable) matrix.

  -- | Like 'thawMatrix', except the 'Matrix' given to this function can no
  --   longer be used, and the performance may be better.
  --
  --   If Haskell had uniqueness types, this function would be safe, as the
  --   function could take a unique 'Matrix', guaranteeing that there are no
  --   copies lingering around to break referential transparency.
  unsafeThawMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => Unique (Matrix p a b)
    -- ^ An immutable matrix to thaw.
    -> m (Unique (MutableMatrix p a b (PrimState m)))
    -- ^ A 'PrimMonad' action returning the thawed (mutable) matrix.

  -- | Get the shape of the given matrix.
  shapeMatrix
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix to get the shape of.
    -> MatrixShape
    -- ^ The shape of that matrix.

  -- | FIXME: doc
  coeffMatrix
    :: MatrixPos
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc
    -> a
    -- ^ FIXME: doc

  -- | FIXME: doc
  unsafeCoeffMatrix
    :: MatrixPos
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc
    -> a
    -- ^ FIXME: doc

  -- | Add two matrices together.
  addMatrix
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ The left hand side of the addition.
    -> Matrix p a b
    -- ^ The right hand side of the addition.
    -> Unique (Matrix p a b)
    -- ^ The sum of the two given matrices.

  -- | Subtract one matrix from another.
  subMatrix
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ The left hand side of the subtraction.
    -> Matrix p a b
    -- ^ The right hand side of the subtraction.
    -> Unique (Matrix p a b)
    -- ^ The difference between the two given matrices.

  -- | Compute the product of two matrices.
  mulMatrix
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ The left hand side of the product.
    -> Matrix p a b
    -- ^ The right hand side of the product.
    -> Unique (Matrix p a b)
    -- ^ The product of the two given matrices.

  -- | Encode an immutable matrix as a lazy bytestring.
  --
  --   Laws:
  --     * @('decodeMatrix' '.' 'encodeMatrix' \@'Sparse') ≡ 'id'@
  --     * @('decodeMatrix' '.' 'encodeMatrix' \@'Dense')  ≡ 'id'@
  encodeMatrix
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ An immutable matrix to encode.
    -> LBS.ByteString
    -- ^ The matrix, encoded as a lazy bytestring.

  -- | Decode an immutable matrix from a lazy bytestring.
  --
  --   WARNING: this function is partial.
  --
  --   Laws:
  --     * @('decodeMatrix' '.' 'encodeMatrix' \@'Sparse') ≡ 'id'@
  --     * @('decodeMatrix' '.' 'encodeMatrix' \@'Dense')  ≡ 'id'@
  decodeMatrix
    :: (Eigen.Elem a b)
    => LBS.ByteString
    -- ^ A matrix encoded as a lazy bytestring with 'encodeMatrix'.
    -> Unique (Matrix p a b)
    -- ^ An immutable matrix decoded from the bytestring.

-- | FIXME: doc
instance IsMatrix 'Dense where
  zeroMatrix       = uncurry Eigen.Matrix.zero
  thawMatrix       = Eigen.Matrix.thaw
  unsafeThawMatrix = Eigen.Matrix.unsafeThaw
  shapeMatrix      = Eigen.Matrix.dims
  addMatrix        = (+)
  subMatrix        = (-)
  mulMatrix        = (*)
  encodeMatrix     = Eigen.Matrix.encode
  decodeMatrix     = Eigen.Matrix.decode


-- | FIXME: doc
instance IsMatrix 'Sparse where
  zeroMatrix (x, y) = Eigen.SparseMatrix.fromList x y []
  thawMatrix matrix = do
    -- FIXME: verify that this is safe
    m <- unsafeIOToPrim (Eigen.SparseMatrix.thaw matrix)
    pure (MSparseMatrix m)
  unsafeThawMatrix matrix = do
    -- FIXME: verify that this is safe
    m <- unsafeIOToPrim (Eigen.SparseMatrix.unsafeThaw matrix)
    pure (MSparseMatrix m)
  shapeMatrix = Eigen.SparseMatrix.rows &&& Eigen.SparseMatrix.cols
  addMatrix    = (+)
  subMatrix    = (-)
  mulMatrix    = (*)
  encodeMatrix = Eigen.SparseMatrix.encode
  decodeMatrix = Eigen.SparseMatrix.decode

--------------------------------------------------------------------------------

-- | FIXME: doc
class IsMutableMatrix (p :: Packing) where
  -- | Create a new 'MutableMatrix' with the given shape.
  newMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MatrixShape
    -- ^ The shape of the 'MutableMatrix' to create.
    -> m (Unique (MutableMatrix p a b (PrimState m)))
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
    -> m (Unique (Matrix p a b))
    -- ^ An immutable 'Matrix'.

  -- | Like 'freezeMutableMatrix', except the 'MutableMatrix' given to this
  --   function can no longer be used, and the performance may be better.
  --
  --   If Haskell had uniqueness types, this function would be safe, as the
  --   function could take a unique 'MutableMatrix', guaranteeing that there
  --   are no copies lingering around to break referential transparency.
  unsafeFreezeMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => Unique (MutableMatrix p a b (PrimState m))
    -- ^ The 'MutableMatrix' to freeze.
    -> m (Unique (Matrix p a b))
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
  newMutableMatrix (x, y) = do
    -- FIXME: verify that this is safe
    m <- unsafeIOToPrim (Eigen.IOSparseMatrix.new x y)
    pure (MSparseMatrix m)
  reserveMutableMatrix space matrix = do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.IOSparseMatrix.reserve m space)
  validMutableMatrix = const True
  getMutableMatrix matrix (x, y) = do
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.IOSparseMatrix.read (coerce matrix) x y)
  setMutableMatrix matrix (x, y) el = do
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.IOSparseMatrix.write (coerce matrix) x y el)
  unsafeGetMutableMatrix = getMutableMatrix
  unsafeSetMutableMatrix = setMutableMatrix
  freezeMutableMatrix matrix = do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.SparseMatrix.freeze m)
  unsafeFreezeMutableMatrix matrix = do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.SparseMatrix.unsafeFreeze m)
  shapeMutableMatrix matrix = do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    r <- unsafeIOToPrim (Eigen.IOSparseMatrix.rows m)
    -- FIXME: verify that this is safe
    c <- unsafeIOToPrim (Eigen.IOSparseMatrix.cols m)
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
  --     toSparse = 'convertMatrix' \@_ \@'Sparse'
  --
  --     fromSparse :: ('Eigen.Elem' a b)
  --                => 'Matrix' 'Sparse' a b -> 'Matrix' p a b
  --     fromSparse = 'convertMatrix' \@'Sparse' \@_
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
isCompressedSparseMutableMatrix matrix = do
  let (MSparseMatrix m) = matrix
  unsafeIOToPrim (Eigen.IOSparseMatrix.compressed m)

-- | Convert the given mutable sparse matrix to compressed mode.
compressSparseMutableMatrix
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ A mutable sparse matrix that may be in compressed mode.
  --
  --   FIXME: is it true that matrices in compressed mode are tolerated?
  -> m ()
  -- ^ A 'PrimMonad' action that converts the matrix to compressed mode.
compressSparseMutableMatrix matrix = do
  let (MSparseMatrix m) = matrix
  unsafeIOToPrim (Eigen.IOSparseMatrix.compress m)

-- | Convert the given mutable sparse matrix to uncompressed mode.
uncompressSparseMutableMatrix
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ A mutable sparse matrix that may be in compressed mode.
  --
  --   FIXME: is it true that matrices not in compressed mode are tolerated?
  -> m ()
  -- ^ A 'PrimMonad' action that converts the matrix to uncompressed mode.
uncompressSparseMutableMatrix matrix = do
  let (MSparseMatrix m) = matrix
  unsafeIOToPrim (Eigen.IOSparseMatrix.uncompress m)

--------------------------------------------------------------------------------

invertSquareMatrix
  :: (Eigen.Elem a b)
  => Matrix 'Dense a b
  -> Maybe (Matrix 'Dense a b)
invertSquareMatrix
  = undefined

--------------------------------------------------------------------------------

-- | Uses the Gershgorin circle theorem to check if the given sparse matrix
--   is positive-definite.
isPositiveDefinite :: (PrimMonad m, Eigen.Elem a b, Ord a)
                   => MutableMatrix 'Sparse a b (PrimState m)
                   -> m Bool
isPositiveDefinite matrix = either id id <$> do
  ExceptT.runExceptT $ do
    let earlyReturn = ExceptT.throwE

    (numRows, numCols) <- lift $ shapeMutableMatrix matrix

    -- A positive-definite matrix is square by definition, so if the given
    -- matrix is not square then we return false.
    when (numRows /= numCols) $ earlyReturn False

    frozen <- lift (uncompressSparseMatrix <$> freezeMutableMatrix matrix)

    -- Check if the matrix is diagonal, and if it is, return early with the
    -- boolean representing whether all entries of the diagonal are positive,
    -- since these are the eigenvalues.
    () <- do
      let list = Eigen.SparseMatrix.toList frozen
      when (all (\(r, c, _) -> r == c) list) $ do
        earlyReturn (all (\(_, _, v) -> v > 0) list)

    () <- do
      -- Compute the Gershgorin circles.
      pairs <- forM [0 .. numRows - 1] $ \i -> do
        (center, radius) <- lift $ do
          c <- unsafeGetMutableMatrix matrix (i, i)
          r <- [ abs <$> unsafeGetMutableMatrix matrix (i, j)
               | j <- [0 .. numCols - 1]
               ] |> sequenceA |> fmap (sum .> (\x -> x - c))
          pure (c, r)

        -- If one of the circles contains only negative values, then there is
        -- at least one negative eigenvalue, so the matrix cannot be positive
        -- definite.
        when (center + radius < 0) $ earlyReturn False

        pure (center, radius)

      -- If none of the circles contain negative values, then all the
      -- eigenvalues must be positive, so the matrix is positive definite.
      when (all (\(c, r) -> (c - r) > 0) pairs) $ earlyReturn True

    undefined

--------------------------------------------------------------------------------

-- LIST OF FUNCTIONS NOT YET COVERED

-- Eigen.Matrix.fromList
-- Eigen.Matrix.toList
-- Eigen.Matrix.fromFlatList
-- Eigen.Matrix.toFlatList
-- Eigen.Matrix.generate
-- Eigen.Matrix.empty
-- Eigen.Matrix.null
-- Eigen.Matrix.square
-- Eigen.Matrix.ones
-- Eigen.Matrix.identity
-- Eigen.Matrix.constant
-- Eigen.Matrix.random
-- Eigen.Matrix.coeff
-- Eigen.Matrix.unsafeCoeff
-- Eigen.Matrix.col
-- Eigen.Matrix.row
-- Eigen.Matrix.block
-- Eigen.Matrix.topRows
-- Eigen.Matrix.bottomRows
-- Eigen.Matrix.leftCols
-- Eigen.Matrix.rightCols
-- Eigen.Matrix.sum
-- Eigen.Matrix.prod
-- Eigen.Matrix.mean
-- Eigen.Matrix.minCoeff
-- Eigen.Matrix.maxCoeff
-- Eigen.Matrix.trace
-- Eigen.Matrix.norm
-- Eigen.Matrix.squaredNorm
-- Eigen.Matrix.blueNorm
-- Eigen.Matrix.hypotNorm
-- Eigen.Matrix.determinant
-- Eigen.Matrix.fold
-- Eigen.Matrix.fold'
-- Eigen.Matrix.ifold
-- Eigen.Matrix.ifold'
-- Eigen.Matrix.fold1
-- Eigen.Matrix.fold1'
-- Eigen.Matrix.all
-- Eigen.Matrix.any
-- Eigen.Matrix.count
-- Eigen.Matrix.map
-- Eigen.Matrix.imap
-- Eigen.Matrix.filter
-- Eigen.Matrix.ifilter
-- Eigen.Matrix.diagonal
-- Eigen.Matrix.transpose
-- Eigen.Matrix.inverse
-- Eigen.Matrix.adjoint
-- Eigen.Matrix.conjugate
-- Eigen.Matrix.normalize
-- Eigen.Matrix.modify
-- Eigen.Matrix.convert
-- Eigen.Matrix.triangularView
-- Eigen.Matrix.lowerTriangle
-- Eigen.Matrix.upperTriangle
-- Eigen.Matrix.unsafeWith

-- Eigen.SparseMatrix.values
-- Eigen.SparseMatrix.innerIndices
-- Eigen.SparseMatrix.outerStarts
-- Eigen.SparseMatrix.innerNNZs
-- Eigen.SparseMatrix.coeff
-- Eigen.SparseMatrix.fromList
-- Eigen.SparseMatrix.toList
-- Eigen.SparseMatrix.fromVector
-- Eigen.SparseMatrix.toVector
-- Eigen.SparseMatrix.fromDenseList
-- Eigen.SparseMatrix.toDenseList
-- Eigen.SparseMatrix.norm
-- Eigen.SparseMatrix.squaredNorm
-- Eigen.SparseMatrix.blueNorm
-- Eigen.SparseMatrix.block
-- Eigen.SparseMatrix.nonZeros
-- Eigen.SparseMatrix.innerSize
-- Eigen.SparseMatrix.outerSize
-- Eigen.SparseMatrix.pruned
-- Eigen.SparseMatrix.scale
-- Eigen.SparseMatrix.transpose
-- Eigen.SparseMatrix.adjoint

-- Eigen.MMatrix.replicate
-- FIXME: more

-- Eigen.IOSparseMatrix.innerSize
-- Eigen.IOSparseMatrix.outerSize
-- Eigen.IOSparseMatrix.nonZeros
-- Eigen.IOSparseMatrix.setZero
-- Eigen.IOSparseMatrix.setIdentity
-- Eigen.IOSparseMatrix.resize
-- Eigen.IOSparseMatrix.conservativeResize

-- Eigen.LA.solve
-- Eigen.LA.relativeError
-- Eigen.LA.rank
-- Eigen.LA.kernel
-- Eigen.LA.image

-- Eigen.SparseLA.runSolverT
-- Eigen.SparseLA.analyzePattern
-- Eigen.SparseLA.factorize
-- Eigen.SparseLA.compute
-- Eigen.SparseLA.solve
-- Eigen.SparseLA.info
-- Eigen.SparseLA.tolerance
-- Eigen.SparseLA.setTolerance
-- Eigen.SparseLA.maxIterations
-- Eigen.SparseLA.setMaxIterations
-- Eigen.SparseLA.error
-- Eigen.SparseLA.iterations
-- Eigen.SparseLA.matrixR
-- Eigen.SparseLA.matrixQ
-- Eigen.SparseLA.rank
-- Eigen.SparseLA.setPivotThreshold
-- Eigen.SparseLA.setSymmetric
-- Eigen.SparseLA.matrixL
-- Eigen.SparseLA.matrixU
-- Eigen.SparseLA.determinant
-- Eigen.SparseLA.absDeterminant
-- Eigen.SparseLA.signDeterminant
-- Eigen.SparseLA.logAbsDeterminant

--------------------------------------------------------------------------------
