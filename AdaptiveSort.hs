--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

--------------------------------------------------------------------------------

module AdaptiveSort where
--  ( -- * FIXME: doc
--    sort
--  , benchmark
--  ) where

--------------------------------------------------------------------------------

import           Control.Monad
import           Control.Monad.Primitive          (PrimMonad (PrimState))

import           Data.Proxy                       (Proxy (Proxy))

import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector
import qualified Data.Vector.Generic.Mutable      as MVector.Generic
import           Data.Vector.Mutable              (MVector)
import qualified Data.Vector.Mutable              as MVector
import           Data.Vector.Unboxed              (Unbox)
import qualified Data.Vector.Unboxed              as UVector
import qualified Data.Vector.Unboxed.Mutable      as UMVector

import qualified Data.Vector.Algorithms.Insertion as MVector.InsertionSort
import           Data.Vector.Algorithms.Radix     (Radix)
import qualified Data.Vector.Algorithms.Radix     as MVector.RadixSort

import           Flow                             ((.>), (|>))

--------------------------------------------------------------------------------

import           Control.DeepSeq                  (NFData, deepseq, force)

import           Control.Monad.ST                 (ST, runST)

import qualified Data.Random                      as RFu
import qualified Data.Random.List                 as RFu
import qualified Data.Random.RVar                 as RFu

import qualified Criterion.Main                   as C

runRVar :: RFu.RVar a -> IO a
runRVar var = RFu.runRVar var RFu.StdRandom

shuffle :: Vector a -> IO (Vector a)
shuffle vec = RFu.shuffleN (Vector.length vec) (Vector.toList vec)
              |> runRVar |> fmap Vector.fromList

benchmark :: forall a.
             ( Adaptive a
             , RFu.Distribution RFu.StdUniform a
             ) => Proxy a -> IO ()
benchmark _ = do
  let benchmarks = [1 .. 100]
  randomVec <- fmap (force @(UVector.Vector a)) $ runRVar $ do
    UVector.fromList <$> replicateM (maximum benchmarks) RFu.stdUniform
  let mkBench :: Int -> C.Benchmark
      mkBench i
        = let vec = UVector.take i randomVec
          in vec
             `deepseq`
             C.bgroup (show i)
             [ C.bench "radix"
               (C.nf (\v -> runST (pure v
                                   >>= UVector.thaw
                                   >>= MVector.RadixSort.sort))
                vec)
             , C.bench "insertion"
               (C.nf (\v -> runST (pure v
                                   >>= UVector.thaw
                                   >>= MVector.InsertionSort.sort))
                vec)
             ]
  C.defaultMain [C.bgroup "sort" (map mkBench benchmarks)]

--------------------------------------------------------------------------------

class (Unbox a, Radix a, Ord a) => Adaptive a where
  adaptiveRadixMinimum :: Proxy a -> Int

instance Adaptive Int where
  adaptiveRadixMinimum _ = 64

--------------------------------------------------------------------------------

sort :: forall a v m.
        ( PrimMonad m, MVector.Generic.MVector v a, Adaptive a
        ) => v (PrimState m) a -> m ()
sort mvector = do
  let size = MVector.Generic.basicLength mvector
  let radixMinimum = adaptiveRadixMinimum (Proxy @a)
  if | (size < radixMinimum) -> MVector.InsertionSort.sort mvector
     | otherwise             -> MVector.RadixSort.sort     mvector

--------------------------------------------------------------------------------
