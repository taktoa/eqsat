--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Internal.MGraph
  ( module EqSat.Internal.MGraph -- FIXME: explicit export list
  ) where

--------------------------------------------------------------------------------

import           Control.Monad

import           Control.Monad.Primitive

import           Data.STRef                (STRef)
import qualified Data.STRef                as STRef

import           Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Maybe as MaybeT

import           Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Class as MonadTrans

import           Data.Maybe

import           Data.Hashable             (Hashable (hashWithSalt))

import qualified Data.Judy                 as Judy

import           GHC.Generics              (Generic)

import           Data.Set                  (Set)
import qualified Data.Set                  as Set

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map

import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IntMap

import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HashSet

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap

import           EqSat.Internal.MHashSet   (MHashSet)
import qualified EqSat.Internal.MHashSet   as MHashSet

import           EqSat.Internal.MHashMap   (MHashMap)
import qualified EqSat.Internal.MHashMap   as MHashMap

import           Data.Vector.Generic       (Mutable, Vector)
import qualified Data.Vector.Generic       as Vector

import qualified Data.Graph

import           Flow                      ((.>), (|>))

--------------------------------------------------------------------------------

-- * 'MGraph'

-- | FIXME: doc
data MGraph s g v e
  = UnsafeMkMGraph
    { _mgraphEdgeMap       :: !(MHashMap s v (MHashMap s v e))
    , _mgraphNodeSet       :: !(MHashSet s v)
    , _mgraphWeightToPairs :: !(MHashMap s e (MHashSet s (v, v)))
    }
  deriving ()

-- | FIXME: doc
newMGraph
  :: (PrimMonad m)
  => m (MGraph (PrimState m) g v e)
  -- ^ FIXME: doc
newMGraph = do
  edgeMap <- MHashMap.new
  nodeSet <- MHashSet.new
  wtpMap  <- MHashMap.new
  pure (UnsafeMkMGraph edgeMap nodeSet wtpMap)

-- | FIXME: doc
numNodesMGraph
  :: (PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ FIXME: doc
  -> m Int
  -- ^ FIXME: doc
numNodesMGraph = _mgraphNodeSet .> MHashSet.length

-- | FIXME: doc
numEdgesMGraph
  :: (PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ FIXME: doc
  -> m Int
  -- ^ FIXME: doc
numEdgesMGraph = _mgraphWeightToPairs .> MHashMap.length

--------------------------------------------------------------------------------

-- * 'SomeMGraph'

-- | FIXME: doc
data SomeMGraph s v e where
  -- | FIXME: doc
  MkSomeMGraph :: MGraph s g v e
               -> SomeMGraph s v e

--------------------------------------------------------------------------------

-- * 'MNode'

-- | FIXME: doc
data MNode s g v e
  = -- | FIXME: doc
    UnsafeMkMNode
    { _mnodeGraph :: !(MGraph s g v e)
      -- ^ FIXME: doc
    , _mnodeLabel :: !v
      -- ^ FIXME: doc
    }

-- | Compares just the labels for equality, not the whole graph.
--   This is okay because the phantom type variable @g@ ensures that the
--   two graphs are the same.
instance (Eq v) => Eq (MNode s g v e) where
  a == b = _mnodeLabel a == _mnodeLabel b

-- | Compares just the labels, not the whole graph.
--   This is okay because the phantom type variable @g@ ensures that the
--   two graphs are the same.
instance (Ord v) => Ord (MNode s g v e) where
  compare a b = compare (_mnodeLabel a) (_mnodeLabel b)

-- | Hashes just the labels, not the whole graph. This is okay because the
--   phantom type variable @g@ ensures that the two graphs are the same.
instance (Hashable v) => Hashable (MNode s g v e) where
  hashWithSalt salt node = hashWithSalt salt (_mnodeLabel node)

-- | FIXME: doc
validMNode
  :: (PrimMonad m)
  => MNode (PrimState m) g v e
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
validMNode = undefined

-- | FIXME: doc
graphMNode
  :: MNode s g v e
  -- ^ FIXME: doc
  -> MGraph s g v e
  -- ^ FIXME: doc
graphMNode = _mnodeGraph

-- | FIXME: doc
labelMNode
  :: MNode s g v e
  -- ^ FIXME: doc
  -> v
  -- ^ FIXME: doc
labelMNode = _mnodeLabel

--------------------------------------------------------------------------------

-- * 'MEdge'

-- | FIXME: doc
data MEdge s g v e
  = UnsafeMkMEdge
    { _medgeGraph  :: !(MGraph s g v e)
    , _medgeSource :: !v
    , _medgeTarget :: !v
    }
  deriving ()

-- | Compares just the source and target labels for equality, not the whole
--   graph. This is okay because the phantom type variable @g@ ensures that
--   the two graphs are the same.
instance (Eq v) => Eq (MEdge s g v e) where
  a == b = pairMEdge a == pairMEdge b

-- | Compares just the source and target labels (in lexicographic order), not
--   the whole graph. This is okay because the phantom type variable @g@
--   ensures that the two graphs are the same.
instance (Ord v) => Ord (MEdge s g v e) where
  compare a b = compare (pairMEdge a) (pairMEdge b)

-- | Hashes just the source and target labels, not the whole graph. This is
--   okay because the phantom type variable @g@ ensures that the two graphs
--   are the same.
instance (Hashable v) => Hashable (MEdge s g v e) where
  hashWithSalt salt edge = salt
                           `hashWithSalt` (_medgeSource edge)
                           `hashWithSalt` (_medgeTarget edge)

-- | FIXME: doc
sourceMEdge
  :: MEdge s g v e
  -- ^ FIXME: doc
  -> MNode s g v e
  -- ^ FIXME: doc
sourceMEdge e = UnsafeMkMNode (_medgeGraph e) (_medgeSource e)

-- | FIXME: doc
targetMEdge
  :: MEdge s g v e
  -- ^ FIXME: doc
  -> MNode s g v e
  -- ^ FIXME: doc
targetMEdge e = UnsafeMkMNode (_medgeGraph e) (_medgeTarget e)

-- | FIXME: doc
pairMEdge
  :: MEdge s g v e
  -- ^ FIXME: doc
  -> (v, v)
  -- ^ FIXME: doc
pairMEdge e = (labelMNode (sourceMEdge e), labelMNode (targetMEdge e))

-- | Returns the weight associated with the given 'MEdge', if it has not yet
--   been deleted from its graph.
getWeightMEdge
  :: (Eq v, Hashable v, PrimMonad m)
  => MEdge (PrimState m) g v e
  -- ^ FIXME: doc
  -> m (Maybe e)
  -- ^ FIXME: doc
getWeightMEdge edge = MaybeT.runMaybeT $ do
  let graph = _medgeGraph edge
  let (source, target) = pairMEdge edge
  let edgeMap = _mgraphEdgeMap graph
  outgoingMap <- MaybeT (MHashMap.lookup edgeMap     source)
  weight      <- MaybeT (MHashMap.lookup outgoingMap target)
  pure weight

-- | FIXME: doc
setWeightMEdge
  :: (Eq v, Eq e, Hashable v, Hashable e, PrimMonad m)
  => MEdge (PrimState m) g v e
  -- ^ FIXME: doc
  -> e
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
setWeightMEdge edge weight = do
  fmap isJust $ MaybeT.runMaybeT $ do
    let graph = _medgeGraph edge
    let (source, target) = pairMEdge edge
    let edgeMap = _mgraphEdgeMap       graph
    let wtpMap  = _mgraphWeightToPairs graph
    outgoingMap <- MaybeT (MHashMap.lookup edgeMap     source)
    oldWeight   <- MaybeT (MHashMap.lookup outgoingMap target)
    pairSet     <- MaybeT (MHashMap.lookup wtpMap      oldWeight)
    MHashMap.insert outgoingMap target weight
    MHashSet.delete pairSet (source, target)
    newPairSet <- do
      hs <- MHashSet.newWithCapacity 1
      MHashSet.insert hs (source, target)
      pure hs
    MHashMap.insertWith wtpMap weight newPairSet $ \old _ -> do
      MHashSet.insert old (source, target)
      pure old

-- | Delete the given 'MEdge' from its 'MGraph'.
deleteMEdge
  :: (Eq v, Hashable v, PrimMonad m)
  => MEdge (PrimState m) g v e
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
deleteMEdge e = do
  undefined

-- | Extract the source, target, and weight of the given 'MEdge', and then
--   delete it from its 'MGraph'.
consumeMEdge
  :: (Eq v, Hashable v, PrimMonad m)
  => MEdge (PrimState m) g v e
  -- ^ FIXME: doc
  -> m (Maybe ((MNode (PrimState m) g v e, MNode (PrimState m) g v e), e))
  -- ^ FIXME: doc
consumeMEdge e = MaybeT.runMaybeT $ do
  let source = sourceMEdge e
  let target = targetMEdge e
  weight <- MaybeT (getWeightMEdge e)
  lift $ deleteMEdge e
  pure ((source, target), weight)

-- | FIXME: doc
findMEdge
  :: (Eq v, Hashable v, PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ FIXME: doc
  -> (v, v)
  -- ^ FIXME: doc
  -> m (Maybe (MEdge (PrimState m) g v e))
  -- ^ FIXME: doc
findMEdge = undefined

--------------------------------------------------------------------------------

-- * Simple subroutines

-- | FIXME: doc
addNode
  :: (Eq v, Hashable v, PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ FIXME: doc
  -> v
  -- ^ FIXME: doc
  -> m (Bool, MNode (PrimState m) g v e)
  -- ^ FIXME: doc
addNode graph node = do
  result <- MHashSet.member (_mgraphNodeSet graph) node
  MHashSet.insert (_mgraphNodeSet graph) node
  targetMap <- MHashMap.new
  MHashMap.insert (_mgraphEdgeMap graph) node targetMap
  pure (result, UnsafeMkMNode graph node)

-- | FIXME: doc
addEdge
  :: (Eq v, Eq e, Hashable v, Hashable e, PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ The graph to mutate.
  -> v
  -- ^ Source node of the edge.
  -> v
  -- ^ Target node of the edge.
  -> e
  -- ^ Weight of the edge.
  -> m (AddEdgeResult, MEdge (PrimState m) g v e)
  -- ^ A 'PrimMonad' action that will mutate the given graph by adding the
  --   source and target nodes if they don't already exist and by adding an
  --   edge with the given weight between them. It will then return
  --   an 'AddEdgeResult' that encodes how the graph was mutated, as well
  --   as an 'MEdge' reference to the created edge.
addEdge graph source target weight = do
  (sourceAdded, _) <- addNode graph source
  (targetAdded, _) <- addNode graph target
  targetMap <- MHashMap.lookup (_mgraphEdgeMap graph) source
               >>= maybe (fail "this should never happen") pure
  edgeAdded <- do
    edgeAddedRef <- stToPrim (STRef.newSTRef True)
    MHashMap.insertWith targetMap target weight $ \_ new -> do
      stToPrim (STRef.writeSTRef edgeAddedRef False)
      pure new
    stToPrim (STRef.readSTRef edgeAddedRef)
  pairsSet <- MHashMap.lookup (_mgraphWeightToPairs graph) weight
              >>= maybe MHashSet.new pure
  MHashSet.insert pairsSet (source, target)
  MHashMap.insert (_mgraphWeightToPairs graph) weight pairsSet
  let aer = if |     sourceAdded && not targetAdded -> AddedSource
               | not sourceAdded &&     targetAdded -> AddedTarget
               |     sourceAdded &&     targetAdded -> AddedSourceAndTarget
               | edgeAdded                          -> AddedEdge
               | otherwise                          -> UpdatedEdge
  pure (aer, UnsafeMkMEdge graph source target)

-- | Return 'True' iff the given graph contains the given node.
memberNode
  :: (Eq v, Hashable v, PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ FIXME: doc
  -> v
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
memberNode graph node = do
  MHashSet.member (_mgraphNodeSet graph) node

-- | FIXME: doc
getEdgesWithWeight
  :: (Eq v, Eq e, Hashable v, Hashable e, PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ FIXME: doc
  -> e
  -- ^ FIXME: doc
  -> m (HashSet (MEdge (PrimState m) g v e))
  -- ^ FIXME: doc
getEdgesWithWeight graph weight = do
  frozen <- MHashMap.lookup (_mgraphWeightToPairs graph) weight
            >>= maybe MHashSet.new pure
            >>= MHashSet.freeze
  pure (HashSet.map (uncurry (UnsafeMkMEdge graph)) frozen)

-- | FIXME: doc
getOutgoingEdges
  :: (Eq v, Eq e, Hashable v, Hashable e, PrimMonad m)
  => MNode (PrimState m) g v e
  -- ^ FIXME: doc
  -> m (HashMap (MEdge (PrimState m) g v e) e)
  -- ^ FIXME: doc
getOutgoingEdges node = do
  let graph = graphMNode node
  fmap (fromMaybe HashMap.empty) $ MaybeT.runMaybeT $ do
    let source = labelMNode node
    -- FIXME: if the given node is not valid, maybe we should return Nothing
    m <- MaybeT (MHashMap.lookup (_mgraphEdgeMap graph) source)
    MHashMap.foldM m HashMap.empty $ \target weight hm -> do
      let edge = UnsafeMkMEdge graph source target
      pure (HashMap.insert edge weight hm)

-- | FIXME: doc
getNodes
  :: (Eq v, Hashable v, PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ FIXME: doc
  -> m (HashSet v)
  -- ^ FIXME: doc
getNodes graph = do
  MHashSet.freeze (_mgraphNodeSet graph)

-- | FIXME: doc
getEdges
  :: (Eq v, Hashable v, PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ FIXME: doc
  -> m (HashMap (v, v) e)
  -- ^ FIXME: doc
getEdges graph = do
  MHashMap.foldM (_mgraphEdgeMap graph) HashMap.empty $ \source m r -> do
    MHashMap.foldM m r
      $ \target weight -> HashMap.insert (source, target) weight .> pure

--------------------------------------------------------------------------------

-- * Algorithms

-- | A strongly-connected component of a graph.
type SCC s v e = SomeMGraph s v e

-- | Tarjan's strongly-connected components algorithm.
tarjanSCC
  :: forall vec m v e g.
     ( PrimMonad m, Vector vec (SCC (PrimState m) v e)
     , Eq v, Eq e, Hashable v, Hashable e
     )
  => MGraph (PrimState m) g v e
  -- ^ The 'MGraph' on which Tarjan's strongly-connected components algorithm
  --   will be run. This function does not mutate the 'MGraph'.
  -> m (vec (SCC (PrimState m) v e))
  -- ^ A 'PrimMonad' action returning a vector containing the strongly-connected
  --   components of the given 'MGraph' in reverse topological order.
tarjanSCC graph = do
  numNodes <- numNodesMGraph graph

  indexRef   <- stToPrim $ STRef.newSTRef (0 :: Int)
  stackRef   <- stToPrim $ STRef.newSTRef ([] :: [v])
  indexMHM   <- MHashMap.newWithCapacity numNodes
  lowLinkMHM <- MHashMap.newWithCapacity numNodes
  onStackMHS <- MHashSet.newWithCapacity numNodes
  resultRef  <- stToPrim $ STRef.newSTRef ([] :: [SCC (PrimState m) v e])

  let modifyIndex :: (Int -> Int) -> m ()
      modifyIndex f = stToPrim $ STRef.modifySTRef' indexRef f
  let getIndex :: m Int
      getIndex = stToPrim $ STRef.readSTRef indexRef
  let pushStack :: v -> m ()
      pushStack x = stToPrim $ STRef.modifySTRef' stackRef (x:)
  let popStack :: m (Maybe v)
      popStack = stToPrim $ do
        STRef.readSTRef stackRef
          >>= (\case []     -> pure Nothing
                     (x:xs) -> do STRef.writeSTRef stackRef xs
                                  pure (Just x))

  edges <- HashMap.toList <$> getEdges graph

  let strongConnect :: v -> m ()
      strongConnect v = do
        getIndex >>= MHashMap.insert indexMHM   v
        getIndex >>= MHashMap.insert lowLinkMHM v
        modifyIndex (+ 1)
        pushStack v
        MHashSet.insert onStackMHS v
        outgoing <- HashMap.keys <$> getOutgoingEdges (UnsafeMkMNode graph v)
        forM_ outgoing $ \edge -> do
          let w = labelMNode (targetMEdge edge)
          wIndexIsDefined <- MHashMap.member indexMHM w
          wOnStack <- MHashSet.member onStackMHS w
          if not wIndexIsDefined
            then (do strongConnect w
                     llv <- MHashMap.lookup lowLinkMHM v
                            >>= maybe (fail "lowLink is undefined!") pure
                     llw <- MHashMap.lookup lowLinkMHM w
                            >>= maybe (fail "lowLink is undefined!") pure
                     MHashMap.insert lowLinkMHM v (min llv llw))
            else (when wOnStack $ do
                     llv <- MHashMap.lookup lowLinkMHM v
                            >>= maybe (fail "lowLink is undefined!") pure
                     iw  <- MHashMap.lookup indexMHM w
                            >>= maybe (fail "index is undefined!") pure
                     MHashMap.insert lowLinkMHM v (min llv iw))
        llv <- MHashMap.lookup lowLinkMHM v
               >>= maybe (fail "lowLink is undefined!") pure
        iv  <- MHashMap.lookup indexMHM v
               >>= maybe (fail "index is undefined!") pure
        when (llv == iv) $ do
          scc <- newMGraph
          let go maybeW = do
                w <- maybe (fail "stack is empty!") pure maybeW
                MHashSet.delete onStackMHS w
                addNode scc w
                unless (v == w) $ do
                  popStack >>= go
          popStack >>= go
          forM_ edges $ \((src, tgt), weight) -> do
            valid <- (&&) <$> memberNode scc src <*> memberNode scc tgt
            when valid $ do
              void (addEdge scc src tgt weight)
          stToPrim (STRef.modifySTRef' resultRef (MkSomeMGraph scc :))

  nodes <- HashSet.toList <$> getNodes graph
  forM_ nodes $ \v -> do
    visited <- MHashMap.member indexMHM v
    unless visited (strongConnect v)

  -- FIXME: use an MVector instead of a list for efficiency
  Vector.fromList <$> stToPrim (STRef.readSTRef resultRef)

--------------------------------------------------------------------------------

freezeToContainersGraph
  :: (Eq v, Hashable v, PrimMonad m)
  => SomeMGraph (PrimState m) v e
  -> m (Data.Graph.Graph, IntMap v, Map (Int, Int) e)
freezeToContainersGraph (MkSomeMGraph graph) = do
  numNodes <- numNodesMGraph graph
  (vm, vmT) <- stToPrim $ do
    labels <- MHashMap.new
    nodes  <- getNodes graph |> fmap (HashSet.toList .> zip [0 .. numNodes - 1])
    forM_ nodes (uncurry (MHashMap.insert labels))
    xs <- HashMap.toList <$> MHashMap.freeze labels
    pure (IntMap.fromList xs, HashMap.fromList (map (\(a, b) -> (b, a)) xs))
  em <- (MHashMap.foldM (_mgraphEdgeMap graph) Map.empty $ \sourceL m r -> do
            MHashMap.foldM m r $ \targetL weight -> do
              let (s, t) = ( vmT HashMap.! sourceL, vmT HashMap.! targetL )
              Map.insert (s, t) weight .> pure)
  let g = Data.Graph.buildG (0, numNodes - 1) (Map.keys em)
  pure (g, vm, em)

--------------------------------------------------------------------------------

-- | FIXME: doc
data AddEdgeResult
  = -- | FIXME: doc
    AddedSource
  | -- | FIXME: doc
    AddedTarget
  | -- | FIXME: doc
    AddedSourceAndTarget
  | -- | FIXME: doc
    AddedEdge
  | -- | FIXME: doc
    UpdatedEdge
  deriving (Eq, Show, Read, Generic)

-- | FIXME: doc
instance Hashable AddEdgeResult

--------------------------------------------------------------------------------
