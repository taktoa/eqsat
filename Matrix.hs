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

type Eigen a = ExceptT SomeException UIO a

ioToEigen :: IO a -> Eigen a
ioToEigen = UIO.fromIO .> ExceptT

--------------------------------------------------------------------------------

setEigenThreads :: Int -> Eigen ()
setEigenThreads = Eigen.Parallel.setNbThreads .> ioToEigen

getEigenThreads :: Eigen Int
getEigenThreads = Eigen.Parallel.getNbThreads |> ioToEigen

--------------------------------------------------------------------------------

newtype MSparseMatrix a b s
  = MSparseMatrix (Eigen.IOSparseMatrix a b)

--------------------------------------------------------------------------------

data Packing = Dense | Sparse

type family Matrix (p :: Packing) = mat | mat -> p where
  Matrix 'Dense  = Eigen.Matrix
  Matrix 'Sparse = Eigen.SparseMatrix

type family MutableMatrix (p :: Packing) = mat | mat -> p where
  MutableMatrix 'Dense  = Eigen.MMatrix
  MutableMatrix 'Sparse = MSparseMatrix

type MatrixPos   = (Int, Int)
type MatrixShape = (Int, Int)

class IsMatrix (p :: Packing) where
  newMatrix
    :: (Eigen.Elem a b)
    => MatrixShape
    -> Matrix p a b
  thawMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => Matrix p a b
    -> m (MutableMatrix p a b (PrimState m))
  unsafeThawMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => Matrix p a b
    -> m (MutableMatrix p a b (PrimState m))

instance IsMatrix 'Dense where
  newMatrix (x, y) = Eigen.Matrix.zero x y
  thawMatrix = Eigen.Matrix.thaw
  unsafeThawMatrix = Eigen.Matrix.unsafeThaw

instance IsMatrix 'Sparse where
  newMatrix = undefined
  thawMatrix matrix = stToPrim $ do
    m <- unsafeIOToST (Eigen.SparseMatrix.thaw matrix)
    pure (MSparseMatrix m)
  unsafeThawMatrix matrix = stToPrim $ do
    m <- unsafeIOToST (Eigen.SparseMatrix.unsafeThaw matrix)
    pure (MSparseMatrix m)

class IsMutableMatrix (p :: Packing) where
  newMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MatrixShape
    -> m (MutableMatrix p a b (PrimState m))
  validMutableMatrix
    :: (Eigen.Elem a b)
    => MutableMatrix p a b s
    -> Bool
  getMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -> MatrixPos
    -> m a
  setMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -> MatrixPos
    -> a
    -> m ()
  freezeMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -> m (Matrix p a b)
  unsafeFreezeMutableMatrix
    :: (PrimMonad m, Eigen.Elem a b)
    => MutableMatrix p a b (PrimState m)
    -> m (Matrix p a b)

instance IsMutableMatrix 'Dense where
  newMutableMatrix (x, y) = Eigen.MMatrix.new x y
  validMutableMatrix = Eigen.MMatrix.valid
  getMutableMatrix matrix (x, y) = Eigen.MMatrix.read  matrix x y
  setMutableMatrix matrix (x, y) = Eigen.MMatrix.write matrix x y
  freezeMutableMatrix = Eigen.Matrix.freeze
  unsafeFreezeMutableMatrix = Eigen.Matrix.unsafeFreeze

instance IsMutableMatrix 'Sparse where
  newMutableMatrix (x, y) = stToPrim $ do
    m <- unsafeIOToST (Eigen.IOSparseMatrix.new x y)
    pure (MSparseMatrix m)
  validMutableMatrix = const True
  getMutableMatrix matrix (x, y) = stToPrim $ do
    unsafeIOToST (Eigen.IOSparseMatrix.read (coerce matrix) x y)
  setMutableMatrix matrix (x, y) el = stToPrim $ do
    unsafeIOToST (Eigen.IOSparseMatrix.write (coerce matrix) x y el)
  freezeMutableMatrix matrix = stToPrim $ do
    let (MSparseMatrix m) = matrix
    unsafeIOToST (Eigen.SparseMatrix.freeze m)
  unsafeFreezeMutableMatrix matrix = stToPrim $ do
    let (MSparseMatrix m) = matrix
    unsafeIOToST (Eigen.SparseMatrix.unsafeFreeze m)



-- Eigen.MMatrix.replicate
-- Eigen.IOSparseMatrix.setZero
-- Eigen.IOSparseMatrix.setIdentity


--------------------------------------------------------------------------------
