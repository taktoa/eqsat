--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

--------------------------------------------------------------------------------

module GraphMatching
  ( module GraphMatching -- FIXME: explicit export list
  ) where

--------------------------------------------------------------------------------

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST                 (ST, runST)

import           Control.Monad.Trans.Class        (MonadTrans (lift))
import qualified Control.Monad.Trans.Class        as MonadTrans
import           Control.Monad.Trans.Maybe        (MaybeT)
import qualified Control.Monad.Trans.Maybe        as MaybeT

import           Data.Proxy                       (Proxy (Proxy))

import           Foreign.Storable                 (Storable)

import qualified Data.Graph.Immutable             as Graph
import qualified Data.Graph.Mutable               as MGraph
import           Data.Graph.Types                 (Graph, MGraph)
import qualified Data.Graph.Types                 as Graph
import qualified Data.Graph.Types                 as MGraph
import qualified Data.Graph.Types.Internal        as Graph.Internal

import           Data.IntMap                      (IntMap)
import qualified Data.IntMap                      as IntMap

import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

import           Data.Eigen.Matrix                (Matrix)
import           Data.Eigen.Matrix.Mutable        (MMatrix)
import           Data.Eigen.SparseMatrix          (SparseMatrix)
import           Data.Eigen.SparseMatrix.Mutable  (IOSparseMatrix)

import qualified Data.Eigen.LA                    as Eigen.LA
import qualified Data.Eigen.Matrix                as Eigen.Matrix
import qualified Data.Eigen.Matrix.Mutable        as Eigen.MMatrix
import qualified Data.Eigen.Parallel              as Eigen.Parallel
import qualified Data.Eigen.SparseLA              as Eigen.SparseLA
import qualified Data.Eigen.SparseMatrix          as Eigen.SparseMatrix
import qualified Data.Eigen.SparseMatrix.Mutable  as Eigen.IOSparseMatrix

import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector
import qualified Data.Vector.Generic.Mutable      as MVector.Generic
import           Data.Vector.Mutable              (MVector)
import qualified Data.Vector.Mutable              as MVector
import qualified Data.Vector.Storable             as SVector
import qualified Data.Vector.Storable.Mutable     as SMVector
import           Data.Vector.Unboxed              (Unbox)
import qualified Data.Vector.Unboxed              as UVector
import qualified Data.Vector.Unboxed.Mutable      as UMVector

import qualified Data.Vector.Algorithms.Insertion as MVector.InsertionSort
import           Data.Vector.Algorithms.Radix     (Radix)
import qualified Data.Vector.Algorithms.Radix     as MVector.RadixSort

import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS

import           Flow                             ((.>), (|>))

import           MutableBitmap                    (MutableBitmap)
import qualified MutableBitmap

import           Matrix
                 (Matrix, MutableMatrix, Packing (Dense, Sparse))
import qualified Matrix

import           Control.Monad.Amb                (AmbT)
import qualified Control.Monad.Amb                as AmbT

--------------------------------------------------------------------------------

guardM :: (MonadPlus m) => m Bool -> m ()
guardM action = action >>= guard

isConsecutive :: Graph g e Int -> Bool
isConsecutive graph = runST $ do
  let graphSize = Graph.sizeInt (Graph.size graph)
  bitmap <- MutableBitmap.new graphSize
  Vector.forM_ (Graph.verticesToVector (Graph.vertices graph))
    $ \v -> MutableBitmap.set bitmap v True
  MutableBitmap.isAllTrue bitmap

-- graphToSparseMatrix
--   ::

-- graphToAdjacencyMatrix_
--   :: (PrimMonad m) => Graph g () Int -> m (Maybe (Matrix I))
-- graphToAdjacencyMatrix_ graph = MaybeT.runMaybeT $ do
--   guard (isConsecutive graph)
--   let numNodes = Graph.sizeInt (Graph.size graph)
--   lift $ makeMatrix (numNodes, numNodes) $ \set -> do
--     flip Graph.traverseEdges_ graph $ \s t sLabel tLabel () -> do
--       undefined
--
-- mgraphToAdjacencyMatrix_ :: (PrimMonad m)
--                          => MGraph (PrimState m) g () Int
--                          -> m (Maybe (Matrix I))
-- mgraphToAdjacencyMatrix_ = Graph.freeze >=> graphToAdjacencyMatrix_

--------------------------------------------------------------------------------
