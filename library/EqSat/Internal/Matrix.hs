--------------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Internal.Matrix
  ( module EqSat.Internal.Matrix -- FIXME: specific export list
  , Eigen.Elem
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
--   to tag whether a matrix is meant to be /dense/ or /sparse/.
--
--   A /dense/ matrix is an in-memory representation of an @m × n@ matrix valued
--   in a semiring @R@ that uses @Θ(m · n · log₂(|R|))@ bits of storage.
--
--   A /sparse/ matrix is an in-memory representation of an @m × n@ matrix @M@
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

-- | This constraint is solvable for any @p ∷ 'Packing'@.
type IsPacking p
  = ( IsMatrix p
    , IsMutableMatrix p
    , ConvertMatrix p 'Dense
    , ConvertMatrix p 'Sparse
    , ConvertMatrix 'Dense  p
    , ConvertMatrix 'Sparse p
    )

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
type MatrixIndex = Int -- FIXME: use `Refined NonNegative Int` instead?

-- | FIXME: doc
type MatrixPos = (MatrixIndex, MatrixIndex) -- FIXME: use newtype instead?

-- | FIXME: doc
type MatrixShape = (MatrixIndex, MatrixIndex) -- FIXME: use newtype instead?

-- | This is used to document that a function consumes its argument (so it must
--   be a unique value).
type Unique a = a

--------------------------------------------------------------------------------

-- | FIXME: doc
class IsMatrix (p :: Packing) where
  -- | Convert a 'Matrix' to a 'MutableMatrix' with the same packing.
  thaw
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
  unsafeThaw
    :: (PrimMonad m, Eigen.Elem a b)
    => Unique (Matrix p a b)
    -- ^ An immutable matrix to thaw.
    -> m (Unique (MutableMatrix p a b (PrimState m)))
    -- ^ A 'PrimMonad' action returning the thawed (mutable) matrix.

  -- | Create a new 'Matrix' filled with zeroes
  --   (as defined by the 'Elem' instance).
  --
  --   Laws:
  --
  --   1. For any @m ∷ 'Matrix' p a b@, if @(r, c) ≡ 'shape' m@,
  --      then @m ≡ 'add' ('zero' ('shape' m)) m@.
  --   2. For any @m ∷ 'Matrix' p a b@, if @(r, c) ≡ 'shape' m@ and @n ∷ Int@
  --      such that @n > 1@,
  --      then @'zero' (n, c) ≡ 'mul' ('zero' (n, r)) m@
  --      and  @'zero' (r, n) ≡ 'mul' m ('zero' (c, n))@.
  zero
    :: (Eigen.Elem a b)
    => MatrixShape
    -- ^ FIXME: doc
    -> Unique (Matrix p a b)
    -- ^ FIXME: doc

  -- | Create a new Kronecker delta / identity 'Matrix'.
  --
  --   Laws:
  --
  --   1. For any @m ∷ 'Matrix' p a b@, if @(r, c) ≡ 'shape' m@,
  --      then @m ≡ 'mul' ('kronecker' (r, r)) m@
  --      and  @m ≡ 'mul' m ('kronecker' (c, c))@.
  kronecker
    :: (Eigen.Elem a b)
    => MatrixShape
    -- ^ The shape of the identity matrix.
    -> Unique (Matrix p a b)
    -- ^ A matrix with the given shape the entries of which are all zeros
    --   except for the diagonal entries.

  -- | Get the shape of the given matrix.
  shape
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix to get the shape of.
    -> MatrixShape
    -- ^ The shape of that matrix.

  -- | FIXME: doc
  coeff
    :: (Eigen.Elem a b)
    => MatrixPos
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc
    -> a
    -- ^ FIXME: doc

  -- | FIXME: doc
  unsafeCoeff
    :: (Eigen.Elem a b)
    => MatrixPos
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc
    -> a
    -- ^ FIXME: doc

  -- | FIXME: doc
  imap
    :: (Eigen.Elem a b)
    => (MatrixPos -> a -> a)
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc

  -- | FIXME: doc
  imapNonZeros
    :: (Eq a, Eigen.Elem a b)
    => (MatrixPos -> a -> a)
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc

  -- | Add two matrices together.
  add
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ The left hand side of the addition.
    -> Matrix p a b
    -- ^ The right hand side of the addition.
    -> Unique (Matrix p a b)
    -- ^ The sum of the two given matrices.

  -- | Subtract one matrix from another.
  sub
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ The left hand side of the subtraction.
    -> Matrix p a b
    -- ^ The right hand side of the subtraction.
    -> Unique (Matrix p a b)
    -- ^ The difference between the two given matrices.

  -- | Compute the product of two matrices.
  mul
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ The left hand side of the product.
    -> Matrix p a b
    -- ^ The right hand side of the product.
    -> Unique (Matrix p a b)
    -- ^ The product of the two given matrices.

  -- | Scale the given matrix by the given scalar.
  scale
    :: (Eigen.Elem a b)
    => a
    -- ^ A scalar to multiply by a matrix.
    -> Matrix p a b
    -- ^ A matrix that will be scaled by the scalar.
    -> Unique (Matrix p a b)
    -- ^ The scaled matrix.

  -- | Transpose a matrix.
  transpose
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix.
    -> Unique (Matrix p a b)
    -- ^ The result of transposing that matrix.

  -- | Compute the adjoint of a matrix.
  adjoint
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix.
    -> Unique (Matrix p a b)
    -- ^ The adjoint of that matrix.

  -- | FIXME: doc
  block
    :: (Eigen.Elem a b)
    => MatrixPos
    -- ^ FIXME: doc
    -> MatrixShape
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc
    -> Matrix p a b
    -- ^ FIXME: doc

  -- | FIXME: doc
  sum
    :: (Eigen.Elem a b)
    => Matrix p a b
    -> a

  -- | FIXME: doc
  prod
    :: (Eigen.Elem a b)
    => Matrix p a b
    -> a

  -- | FIXME: doc
  mean
    :: (Eigen.Elem a b, Fractional a)
    => Matrix p a b
    -> a

  -- | FIXME: doc
  minCoeff
    :: (Eigen.Elem a b, Ord a)
    => Matrix p a b
    -> a

  -- | FIXME: doc
  maxCoeff
    :: (Eigen.Elem a b, Ord a)
    => Matrix p a b
    -> a

  -- | The trace of a matrix is the sum of the diagonal coefficients.
  --
  --   Laws:
  --
  --   1. FIXME: add laws
  trace
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix or vector.
    -> a
    -- ^ The sum of the diagonal entries.

  -- | For vectors, the L₂ norm, and for matrices, the Frobenius norm.
  --   In both cases, it is just the square root of the sum of the squares of
  --   all the matrix entries. For vectors, this happens to be equal to the
  --   square root of the dot product of the vector with itself.
  --
  --   Laws:
  --
  --   1. FIXME: add laws
  norm
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix or vector.
    -> a
    -- ^ The square root of the sum of the squares of all the entries.

  -- | For vectors, the squared L₂ norm, and for matrices the Frobenius norm.
  --   In both cases, it is just the sum of the squares of all the matrix
  --   entries. For vectors, this happens to be equal to the dot product of
  --   the vector with itself.
  --
  --   Laws:
  --
  --   1. FIXME: add laws
  squaredNorm
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix or vector.
    -> a
    -- ^ The sum of the squares of all the entries.

  -- | The L₂ norm of a matrix using Blue's algorithm.
  --
  --   See /A Portable Fortran Program to Find the Euclidean Norm of a Vector/
  --   in ACM TOMS, Vol 4, Issue 1, 1978.
  blueNorm
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix or vector.
    -> a
    -- ^ The L₂ norm of the matrix or vector, computed with Blue's algorithm.

  -- | FIXME: doc
  determinant
    :: (Eigen.Elem a b)
    => Matrix p a b
    -- ^ A matrix or vector.
    -> a
    -- ^ The determinant of the matrix.

  -- | FIXME: doc
  all
    :: (Eigen.Elem a b)
    => (a -> Bool)
    -> Matrix p a b
    -> Bool

  -- | FIXME: doc
  any
    :: (Eigen.Elem a b)
    => (a -> Bool)
    -> Matrix p a b
    -> Bool

  -- | FIXME: doc
  count
    :: (Eigen.Elem a b)
    => (a -> Bool)
    -> Matrix p a b
    -> Int

  -- | Encode an immutable matrix as a lazy bytestring.
  --
  --   Laws:
  --
  --   1. For any @p ∷ 'Packing'@,
  --      @('decodeMatrix' '.' 'encodeMatrix' \@p) ≡ 'id'@.
  encode
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
  --
  --   1. For any @p ∷ 'Packing'@,
  --      @('decodeMatrix' '.' 'encodeMatrix' \@p) ≡ 'id'@.
  decode
    :: (Eigen.Elem a b)
    => LBS.ByteString
    -- ^ A matrix encoded as a lazy bytestring with 'encodeMatrix'.
    -> Unique (Matrix p a b)
    -- ^ An immutable matrix decoded from the bytestring.

-- | FIXME: doc
instance IsMatrix 'Dense where
  thaw         = Eigen.Matrix.thaw
  unsafeThaw   = Eigen.Matrix.unsafeThaw
  zero         = uncurry Eigen.Matrix.zero
  kronecker    = uncurry Eigen.Matrix.identity
  shape        = Eigen.Matrix.dims
  imap         = (\f -> Eigen.Matrix.imap (curry f))
  imapNonZeros = (\f -> Eigen.Matrix.imap (\r c x -> if x == 0
                                                     then 0
                                                     else f (r, c) x))
  add          = (+)
  sub          = (-)
  mul          = (*)
  scale        = (\s -> Eigen.Matrix.map (* s))
  transpose    = Eigen.Matrix.transpose
  adjoint      = Eigen.Matrix.adjoint
  block        = (\(sr, sc) (br, bc) -> Eigen.Matrix.block sr sc br bc)
  sum          = Eigen.Matrix.sum
  prod         = Eigen.Matrix.prod
  mean         = Eigen.Matrix.mean
  minCoeff     = Eigen.Matrix.minCoeff
  maxCoeff     = Eigen.Matrix.maxCoeff
  trace        = Eigen.Matrix.trace
  norm         = Eigen.Matrix.norm
  squaredNorm  = Eigen.Matrix.squaredNorm
  blueNorm     = Eigen.Matrix.blueNorm
  determinant  = Eigen.Matrix.determinant
  all          = Eigen.Matrix.all
  any          = Eigen.Matrix.any
  count        = Eigen.Matrix.count
  encode       = Eigen.Matrix.encode
  decode       = Eigen.Matrix.decode
  coeff        = uncurry Eigen.Matrix.coeff
  unsafeCoeff  = uncurry Eigen.Matrix.unsafeCoeff

-- | FIXME: doc
instance IsMatrix 'Sparse where
  thaw matrix = do
    -- FIXME: verify that this is safe
    m <- unsafeIOToPrim (Eigen.SparseMatrix.thaw matrix)
    pure (MSparseMatrix m)
  unsafeThaw matrix = do
    -- FIXME: verify that this is safe
    m <- unsafeIOToPrim (Eigen.SparseMatrix.unsafeThaw matrix)
    pure (MSparseMatrix m)
  zero         = (\(x, y) -> Eigen.SparseMatrix.fromList x y [])
  kronecker    = kronecker @'Dense .> Eigen.SparseMatrix.fromMatrix
                 -- FIXME: this could potentially be faster
  shape        = Eigen.SparseMatrix.rows &&& Eigen.SparseMatrix.cols
  imap         = (\f -> convert .> imap @'Dense f .> convert)
  imapNonZeros = (\f m -> Eigen.SparseMatrix.uncompress m
                          |> Eigen.SparseMatrix.toList
                          |> map (\(r, c, x) -> (r, c, f (r, c) x))
                          |> (Eigen.SparseMatrix.fromList
                              (Eigen.SparseMatrix.rows m)
                              (Eigen.SparseMatrix.cols m)))
  add          = (+)
  sub          = (-)
  mul          = (*)
  scale        = Eigen.SparseMatrix.scale
  transpose    = Eigen.SparseMatrix.transpose
  adjoint      = Eigen.SparseMatrix.adjoint
  block        = (\(sr, sc) (br, bc) -> Eigen.SparseMatrix.block sr sc br bc)
  sum          = Eigen.SparseMatrix.toList
                 .> map (\(_, _, x) -> x)
                 .> Prelude.sum
  prod         = (\m -> if hasZeros m
                        then 0
                        else Eigen.SparseMatrix.toList m
                             |> map (\(_, _, x) -> x)
                             |> Prelude.product)
  mean         = (\m -> let (r, c) = shape m
                            scaled = scale (1 / fromIntegral r) m
                        in EqSat.Internal.Matrix.sum scaled / fromIntegral c)
                 -- FIXME: numerical stability / speed questionable
  minCoeff     = Eigen.SparseMatrix.toList
                 .> map (\(_, _, x) -> x)
                 .> (0 :)
                 .> minimum
  maxCoeff     = Eigen.SparseMatrix.toList
                 .> map (\(_, _, x) -> x)
                 .> (0 :)
                 .> maximum
  trace        = (\m -> [0 .. uncurry min (shape m) - 1]
                        |> map (\i -> coeff (i, i) m)
                        |> Prelude.sum)
                 -- FIXME: this could potentially be faster
  norm         = Eigen.SparseMatrix.norm
  squaredNorm  = Eigen.SparseMatrix.squaredNorm
  blueNorm     = Eigen.SparseMatrix.blueNorm
  determinant  = convert .> determinant @'Dense
                 -- FIXME: this could potentially be faster
  all          = (\f m -> Eigen.SparseMatrix.toList m
                          |> Prelude.all (\(_, _, x) -> f x)
                          |> (\b -> if hasZeros m then f 0 && b else b))
  any          = (\f m -> Eigen.SparseMatrix.toList m
                          |> Prelude.any (\(_, _, x) -> f x)
                          |> (\b -> if hasZeros m then f 0 || b else b))
  count        = (\f m -> Eigen.SparseMatrix.toList m
                          |> map (\(_, _, x) -> fromEnum (f x))
                          |> Prelude.sum
                          |> (+ if hasZeros m then fromEnum (f 0) else 0))
  encode       = Eigen.SparseMatrix.encode
  decode       = Eigen.SparseMatrix.decode
  coeff        = uncurry Eigen.SparseMatrix.coeff
  unsafeCoeff  = uncurry Eigen.SparseMatrix.coeff

--------------------------------------------------------------------------------

-- | FIXME: doc
class IsMutableMatrix (p :: Packing) where
  -- | Create a new 'MutableMatrix' with the given shape.
  newMutable
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
  reserveMutable
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
  validMutable
    :: (Eigen.Elem a b)
    => MutableMatrix p a b s
    -- ^ The 'MutableMatrix' to check the validity of.
    -> Bool
    -- ^ 'True' if the given matrix is valid, 'False' otherwise.

  -- | Get the element of the given matrix at the given position.
  --
  --   This function will throw an exception if the position is out of bounds.
  getMutable
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
  unsafeGetMutable
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
  setMutable
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
  unsafeSetMutable
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
  freezeMutable
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
  unsafeFreezeMutable
    :: (PrimMonad m, Eigen.Elem a b)
    => Unique (MutableMatrix p a b (PrimState m))
    -- ^ The 'MutableMatrix' to freeze.
    -> m (Unique (Matrix p a b))
    -- ^ An immutable 'Matrix'.

  -- | Returns the shape of the given 'MutableMatrix'.
  shapeMutable
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -- ^ A 'MutableMatrix' to find the shape of.
    -> m MatrixShape
    -- ^ A 'PrimMonad' action returning the shape of the given matrix.

-- | FIXME: doc
instance IsMutableMatrix 'Dense where
  newMutable          = uncurry Eigen.MMatrix.new
  reserveMutable _ _  = pure ()
  validMutable        = Eigen.MMatrix.valid
  getMutable          = Eigen.MMatrix.read  .> uncurry
  setMutable          = Eigen.MMatrix.write .> uncurry
  unsafeGetMutable    = Eigen.MMatrix.unsafeRead  .> uncurry
  unsafeSetMutable    = Eigen.MMatrix.unsafeWrite .> uncurry
  freezeMutable       = Eigen.Matrix.freeze
  unsafeFreezeMutable = Eigen.Matrix.unsafeFreeze
  shapeMutable        = (Eigen.MMatrix.mm_rows &&& Eigen.MMatrix.mm_cols)
                        .> pure

-- | FIXME: doc
instance IsMutableMatrix 'Sparse where
  newMutable (x, y) = do
    -- FIXME: verify that this is safe
    m <- unsafeIOToPrim (Eigen.IOSparseMatrix.new x y)
    pure (MSparseMatrix m)
  reserveMutable space matrix = do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.IOSparseMatrix.reserve m space)
  validMutable = const True
  getMutable matrix (x, y) = do
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.IOSparseMatrix.read (coerce matrix) x y)
  setMutable matrix (x, y) el = do
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.IOSparseMatrix.write (coerce matrix) x y el)
  unsafeGetMutable = getMutable
  unsafeSetMutable = setMutable
  freezeMutable matrix = do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.SparseMatrix.freeze m)
  unsafeFreezeMutable matrix = do
    let (MSparseMatrix m) = matrix
    -- FIXME: verify that this is safe
    unsafeIOToPrim (Eigen.SparseMatrix.unsafeFreeze m)
  shapeMutable matrix = do
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
  --     toSparse ∷ ('Eigen.Elem' a b)
  --              ⇒ 'Matrix' p a b → 'Matrix' 'Sparse' a b
  --     toSparse = 'convertMatrix' \@_ \@'Sparse'
  --
  --     fromSparse ∷ ('Eigen.Elem' a b)
  --                ⇒ 'Matrix' 'Sparse' a b → 'Matrix' p a b
  --     fromSparse = 'convertMatrix' \@'Sparse' \@_
  --   @
  convert
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
  convertMutable
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p1 a b (PrimState m)
    -- ^ A mutable matrix to convert the representation of.
    -> m (MutableMatrix p2 a b (PrimState m))
    -- ^ A new mutable matrix with the new representation.

-- | Trivial; @'convertMatrix' = 'id'@ and @'convertMutableMatrix' = 'pure'@.
instance ConvertMatrix 'Dense 'Dense where
  convert = id
  convertMutable = pure

-- | Trivial; @'convertMatrix' = 'id'@ and @'convertMutableMatrix' = 'pure'@.
instance ConvertMatrix 'Sparse 'Sparse where
  convert = id
  convertMutable = pure

-- | Nontrivial; 'convertMatrix' uses 'Eigen.SparseMatrix.fromMatrix'
--   and 'convertMutableMatrix' delegates to 'convertMatrix' by freezing,
--   converting, and then thawing.
instance ConvertMatrix 'Dense 'Sparse where
  convert = Eigen.SparseMatrix.fromMatrix
  convertMutable = freezeMutable
                   >=> (convert .> pure)
                   >=> unsafeThaw

-- | Nontrivial; 'convertMatrix' uses 'Eigen.SparseMatrix.toMatrix'
--   and 'convertMutableMatrix' delegates to 'convertMatrix' by freezing,
--   converting, and then thawing.
instance ConvertMatrix 'Sparse 'Dense where
  convert = Eigen.SparseMatrix.toMatrix
  convertMutable = freezeMutable
                   >=> (convert .> pure)
                   >=> unsafeThaw

--------------------------------------------------------------------------------

-- | FIXME: doc
constant
  :: (Eigen.Elem a b)
  => MatrixShape
  -- ^ FIXME: doc
  -> a
  -- ^ FIXME: doc
  -> Matrix 'Dense a b
  -- ^ FIXME: doc
constant = uncurry Eigen.Matrix.constant

-- | FIXME: doc
constantMutable
  :: (Eigen.Elem a b, PrimMonad m)
  => MatrixShape
  -- ^ FIXME: doc
  -> a
  -- ^ FIXME: doc
  -> m (MutableMatrix 'Dense a b (PrimState m))
  -- ^ FIXME: doc
constantMutable sh = constant sh .> unsafeThaw

-- | FIXME: doc
ones
  :: (Eigen.Elem a b)
  => MatrixShape
  -- ^ FIXME: doc
  -> Matrix 'Dense a b
  -- ^ FIXME: doc
ones sh = constant sh 1

-- | FIXME: doc
onesMutable
  :: (Eigen.Elem a b, PrimMonad m)
  => MatrixShape
  -- ^ FIXME: doc
  -> m (MutableMatrix 'Dense a b (PrimState m))
  -- ^ FIXME: doc
onesMutable sh = constantMutable sh 1

--------------------------------------------------------------------------------

-- | Suppresses all nonzeros in the given matrix that are much smaller than the
--   given tolerence value @ε@.
sparsePrune
  :: (Eigen.Elem a b)
  => a
  -- ^ The tolerance value @ε@.
  -> Matrix 'Sparse a b
  -- ^ A sparse matrix.
  -> Matrix 'Sparse a b
  -- ^ The given sparse matrix, with all nonzero elements smaller than @ε@
  --   removed (i.e.: set to zero.)
sparsePrune = Eigen.SparseMatrix.pruned

-- | Checks whether the given immutable sparse matrix is in compressed mode.
sparseIsCompressed
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -- ^ An immutable sparse matrix.
  -> Bool
  -- ^ 'True' if the matrix was in compressed mode, 'False' otherwise.
sparseIsCompressed = Eigen.SparseMatrix.compressed

-- | Convert the given immutable sparse matrix to compressed mode.
sparseCompress
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -- ^ An immutable sparse matrix that may not be in compressed mode.
  --
  --   FIXME: is it true that matrices in compressed mode are tolerated?
  -> Matrix 'Sparse a b
  -- ^ An immutable sparse matrix in compressed mode with the same data.
sparseCompress = Eigen.SparseMatrix.compress

-- | Convert the given immutable sparse matrix to uncompressed mode.
sparseUncompress
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -- ^ An immutable sparse matrix that may be in compressed mode.
  --
  --   FIXME: is it true that matrices not in compressed mode are tolerated?
  -> Matrix 'Sparse a b
  -- ^ An immutable sparse matrix in uncompressed mode with the same data.
sparseUncompress = Eigen.SparseMatrix.uncompress

--------------------------------------------------------------------------------

-- | Checks whether the given mutable sparse matrix is in compressed mode.
sparseMutableIsCompressed
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ A mutable sparse matrix.
  -> m Bool
  -- ^ A 'PrimMonad' action returning 'True' if the given matrix was in
  --   compressed mode and 'False' otherwise.
sparseMutableIsCompressed
  = coerce
    .> Eigen.IOSparseMatrix.compressed
    .> unsafeIOToPrim

-- | Convert the given mutable sparse matrix to compressed mode.
sparseMutableCompress
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ A mutable sparse matrix that may be in compressed mode.
  --
  --   FIXME: is it true that matrices in compressed mode are tolerated?
  -> m ()
  -- ^ A 'PrimMonad' action that converts the matrix to compressed mode.
sparseMutableCompress
  = coerce
    .> Eigen.IOSparseMatrix.compress
    .> unsafeIOToPrim

-- | Convert the given mutable sparse matrix to uncompressed mode.
sparseMutableUncompress
  :: (PrimMonad m, Eigen.Elem a b)
  => MutableMatrix 'Sparse a b (PrimState m)
  -- ^ A mutable sparse matrix that may be in compressed mode.
  --
  --   FIXME: is it true that matrices not in compressed mode are tolerated?
  -> m ()
  -- ^ A 'PrimMonad' action that converts the matrix to uncompressed mode.
sparseMutableUncompress
  = coerce
    .> Eigen.IOSparseMatrix.uncompress
    .> unsafeIOToPrim

--------------------------------------------------------------------------------

-- | FIXME: doc
invertSquareMatrix
  :: (Eigen.Elem a b)
  => Matrix 'Dense a b
  -> Maybe (Matrix 'Dense a b)
invertSquareMatrix
  = undefined

--------------------------------------------------------------------------------

-- | Uses the Gershgorin circle theorem to check if the given sparse matrix
--   is positive-definite.
isPositiveDefinite
  :: (PrimMonad m, Eigen.Elem a b, Ord a)
  => MutableMatrix 'Sparse a b (PrimState m)
  -> m Bool
isPositiveDefinite matrix = either id id <$> do
  ExceptT.runExceptT $ do
    let earlyReturn = ExceptT.throwE

    (numRows, numCols) <- lift $ shapeMutable matrix

    -- A positive-definite matrix is square by definition, so if the given
    -- matrix is not square then we return false.
    when (numRows /= numCols) $ earlyReturn False

    frozen <- lift (sparseUncompress <$> freezeMutable matrix)

    -- Check if the matrix is diagonal, and if it is, return early with the
    -- boolean representing whether all entries of the diagonal are positive,
    -- since these are the eigenvalues.
    () <- do
      let list = Eigen.SparseMatrix.toList frozen
      when (Prelude.all (\(r, c, _) -> r == c) list) $ do
        earlyReturn (Prelude.all (\(_, _, v) -> v > 0) list)

    () <- do
      -- Compute the Gershgorin circles.
      pairs <- forM [0 .. numRows - 1] $ \i -> do
        (center, radius) <- lift $ do
          c <- unsafeGetMutable matrix (i, i)
          r <- [ abs <$> unsafeGetMutable matrix (i, j)
               | j <- [0 .. numCols - 1]
               ] |> sequenceA |> fmap (Prelude.sum .> (\x -> x - c))
          pure (c, r)

        -- If one of the circles contains only negative values, then there is
        -- at least one negative eigenvalue, so the matrix cannot be positive
        -- definite.
        when (center + radius < 0) $ earlyReturn False

        pure (center, radius)

      -- If none of the circles contain negative values, then all the
      -- eigenvalues must be positive, so the matrix is positive definite.
      when (Prelude.all (\(c, r) -> (c - r) > 0) pairs) $ earlyReturn True

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
-- Eigen.Matrix.random
-- Eigen.Matrix.col
-- Eigen.Matrix.row
-- Eigen.Matrix.topRows
-- Eigen.Matrix.bottomRows
-- Eigen.Matrix.leftCols
-- Eigen.Matrix.rightCols
-- Eigen.Matrix.hypotNorm
-- Eigen.Matrix.determinant
-- Eigen.Matrix.fold
-- Eigen.Matrix.fold'
-- Eigen.Matrix.ifold
-- Eigen.Matrix.ifold'
-- Eigen.Matrix.fold1
-- Eigen.Matrix.fold1'
-- Eigen.Matrix.filter
-- Eigen.Matrix.ifilter
-- Eigen.Matrix.diagonal
-- Eigen.Matrix.inverse
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
-- Eigen.SparseMatrix.nonZeros
-- Eigen.SparseMatrix.innerSize
-- Eigen.SparseMatrix.outerSize
-- Eigen.SparseMatrix.scale

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

-- Private helper functions

hasZeros
  :: (Eigen.Elem a b)
  => Matrix 'Sparse a b
  -> Bool
hasZeros m = let (r, c) = shape m
                 numZeros = r * c - Eigen.SparseMatrix.nonZeros m
             in if | (numZeros > 0) -> True
                   | (numZeros < 0) -> error "hasZeros: assertion"
                   | otherwise      -> False

--------------------------------------------------------------------------------
