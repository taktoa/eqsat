--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

--------------------------------------------------------------------------------

module EqSat.Internal.MGraph.Gen
  ( MGraphAction
  , makeMGraphAction
  , emptyMGraphAction
  , composeMGraphAction
  , applyMGraphAction
  , addRandomEdge
  , genMGraph
  ) where

--------------------------------------------------------------------------------

import           EqSat.Internal.MGraph   (MEdge, MGraph, MNode)
import qualified EqSat.Internal.MGraph   as MGraph

import qualified EqSat.Tests.Gen.Misc    as Gen
import qualified Hedgehog                as HH
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range

import           Control.Monad

import           Control.Monad.Primitive

import           Control.Monad.ST.Strict (ST, runST)

import           Data.Text               (Text)

import           Data.Vector             (Vector, (!))
import qualified Data.Vector             as Vector

import           Data.Hashable           (Hashable)

import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as HashSet

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap

import           Data.Proxy              (Proxy (Proxy))

import           Flow                    ((.>), (|>))

--------------------------------------------------------------------------------

data MGraphAction v e
  = MkMGraphAction
    { _MGraphAction_names  :: !(Vector Text)
    , _MGraphAction_action :: !(forall s g. MGraph s g v e -> ST s ())
    }
  deriving ()

instance Show (MGraphAction v e) where
  show = _MGraphAction_names .> show .> (\x -> "(MGraphAction " ++ x ++ ")")

makeMGraphAction
  :: Text
  -> (forall s g. MGraph s g v e -> ST s ())
  -> MGraphAction v e
makeMGraphAction name action = MkMGraphAction (Vector.singleton name) action

emptyMGraphAction
  :: MGraphAction v e
emptyMGraphAction
  = MkMGraphAction
    { _MGraphAction_names  = Vector.empty
    , _MGraphAction_action = (\_ -> pure ())
    }

composeMGraphAction
  :: MGraphAction v e
  -> MGraphAction v e
  -> MGraphAction v e
composeMGraphAction actionA actionB
  = MkMGraphAction
    { _MGraphAction_names  = [ _MGraphAction_names actionA
                              , _MGraphAction_names actionB
                              ] |> mconcat
    , _MGraphAction_action = (\graph -> do
                                  _MGraphAction_action actionA graph
                                  _MGraphAction_action actionB graph)
    }

applyMGraphAction
  :: MGraphAction v e
  -> MGraph s g v e
  -> ST s ()
applyMGraphAction = _MGraphAction_action

--------------------------------------------------------------------------------

-- | FIXME: doc
addRandomEdge
  :: (Eq v, Eq e, Hashable v, Hashable e, HH.MonadGen m)
  => m e
  -- ^ FIXME: doc
  -> m (MGraphAction v e)
  -- ^ FIXME: doc
addRandomEdge randomWeight = do
  sourceN <- Gen.int (Range.constant 0 maxBound)
  targetN <- Gen.int (Range.constant 0 maxBound)
  weight  <- randomWeight
  pure $ makeMGraphAction "addRandomEdge" $ \graph -> do
    nodes <- MGraph.getNodes graph
             |> fmap (HashSet.toList .> Vector.fromList)
    numNodes <- MGraph.numNodesMGraph graph
    let source = nodes ! (sourceN `mod` numNodes)
    let target = nodes ! (targetN `mod` numNodes)
    void $ MGraph.addEdge graph source target weight

-- | FIXME: doc
genMGraph
  :: (Eq v, Eq e, Hashable v, Hashable e, HH.MonadGen m)
  => m v
  -- ^ FIXME: doc
  -> m e
  -- ^ FIXME: doc
  -> m (MGraphAction v e)
  -- ^ FIXME: doc
genMGraph randomLabel randomWeight = do
  size       <- Gen.int (Range.linear 1 100)
  density    <- Gen.int (Range.linear 1 ((size * size) `div` 10))
  labels     <- Vector.replicateM size randomLabel
  edgeAdders <- Vector.replicateM density (addRandomEdge randomWeight)
  pure $ makeMGraphAction "genMGraph" $ \graph -> do
    forM_ [0 .. size - 1] $ \i -> do
      MGraph.addNode graph (labels ! i)
    forM_ [0 .. density - 1] $ \i -> do
      applyMGraphAction (edgeAdders ! i) graph

--------------------------------------------------------------------------------
