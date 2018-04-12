{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module GraphMatching where

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

import           Numeric.LinearAlgebra            (I, Matrix)
import qualified Numeric.LinearAlgebra            as Matrix

import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector
import qualified Data.Vector.Generic.Mutable      as MVector.Generic
import           Data.Vector.Mutable              (MVector)
import qualified Data.Vector.Mutable              as MVector
import           Data.Vector.Unboxed              (Unbox)
import qualified Data.Vector.Unboxed.Mutable      as UMVector

import qualified Data.Vector.Algorithms.Insertion as MVector.InsertionSort
import           Data.Vector.Algorithms.Radix     (Radix)
import qualified Data.Vector.Algorithms.Radix     as MVector.RadixSort

import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS

import           Flow                             ((.>), (|>))

import qualified AdaptiveSort

import           MutableBitmap                    (MutableBitmap)
import qualified MutableBitmap


type UMVector s a = UMVector.MVector s a

guardM :: (MonadPlus m) => m Bool -> m ()
guardM action = action >>= guard

isConsecutive :: Graph g e Int -> Bool
isConsecutive graph = runST $ do
  let graphSize = Graph.sizeInt (Graph.size graph)
  bitmap <- MutableBitmap.new graphSize
  Vector.forM_ (Graph.verticesToVector (Graph.vertices graph))
    $ \v -> MutableBitmap.set bitmap v True
  MutableBitmap.isAllTrue bitmap

makeMatrix_ :: (Storable a, PrimMonad m)
            => (Int, Int)
            -> (MVector (PrimState m) a -> m any)
            -> m (Matrix a)
makeMatrix_ (rows, cols) cb = do
  entries <- MVector.new (rows * cols)
  cb entries
  Vector.freeze entries
    |> fmap (Vector.toList .> (rows Matrix.>< cols))

makeMatrix :: (Storable a, PrimMonad m)
           => (Int, Int)
           -> (((Int, Int) -> a -> m ()) -> m any)
           -> m (Matrix a)
makeMatrix (rows, cols) cb = do
  makeMatrix_ (rows, cols) $ \entries -> do
    cb (\(r, c) -> MVector.write entries (r * cols + c))

unsafeGraphToAdjacencyMatrix_
  :: forall m g. (PrimMonad m) => Graph g () Int -> m (Matrix I)
unsafeGraphToAdjacencyMatrix_ graph = do
  let numNodes = Graph.sizeInt (Graph.size graph)
  makeMatrix (numNodes, numNodes) $ \set -> do
    flip Graph.traverseEdges_ graph $ \s t sLabel tLabel () -> do
      undefined

graphToAdjacencyMatrix_
  :: forall m g. (PrimMonad m) => Graph g () Int -> m (Maybe (Matrix I))
graphToAdjacencyMatrix_ graph = MaybeT.runMaybeT $ do
  guard (isConsecutive graph)
  lift $ unsafeGraphToAdjacencyMatrix_ graph

mgraphToAdjacencyMatrix_ :: (PrimMonad m)
                         => MGraph (PrimState m) g () Int
                         -> m (Maybe (Matrix I))
mgraphToAdjacencyMatrix_ = Graph.freeze >=> graphToAdjacencyMatrix_

main :: IO ()
main = do
  pure ()
