--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Internal.MGraph
  ( module EqSat.Internal.MGraph -- FIXME: explicit export list
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.Primitive

import           Data.STRef                (STRef)
import qualified Data.STRef                as STRef

import           Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Maybe as MaybeT

import           Data.Hashable             (Hashable (hashWithSalt))

import qualified Data.Judy                 as Judy

import           GHC.Generics              (Generic)

import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HashSet

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap

import           EqSat.Internal.MHashSet   (MHashSet)
import qualified EqSat.Internal.MHashSet   as MHashSet

import           EqSat.Internal.MHashMap   (MHashMap)
import qualified EqSat.Internal.MHashMap   as MHashMap

--------------------------------------------------------------------------------

-- | FIXME: doc
data MGraph s g v e
  = UnsafeMkMGraph
    { _mgraphEdgeMap       :: !(MHashMap s v (MHashMap s v e))
    , _mgraphNodeSet       :: !(MHashSet s v)
    , _mgraphWeightToPairs :: !(MHashMap s e (MHashSet s (v, v)))
    }
  deriving ()

--------------------------------------------------------------------------------

-- | FIXME: doc
data MNode s g v e
  = UnsafeMkMNode
    { _mnodeGraph :: !(MGraph s g v e)
    , _mnodeLabel :: !v
    }
  deriving ()

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

-- | FIXME: doc
getWeightMEdge
  :: (Eq v, Hashable v, PrimMonad m)
  => MEdge (PrimState m) g v e
  -- ^ FIXME: doc
  -> m e
  -- ^ FIXME: doc
getWeightMEdge e = do
  undefined

-- | FIXME: doc
setWeightMEdge
  :: (Eq v, Hashable v, PrimMonad m)
  => MEdge (PrimState m) g v e
  -- ^ FIXME: doc
  -> e
  -- ^ FIXME: doc
  -> m Bool
  -- ^ FIXME: doc
setWeightMEdge e = do
  undefined

-- | FIXME: doc
deleteMEdge
  :: (Eq v, Hashable v, PrimMonad m)
  => MEdge (PrimState m) g v e
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
deleteMEdge e = do
  undefined

-- | FIXME: doc
consumeMEdge
  :: (Eq v, Hashable v, PrimMonad m)
  => MEdge (PrimState m) g v e
  -- ^ FIXME: doc
  -> m ((MNode s g v e, MNode s g v e), e)
  -- ^ FIXME: doc
consumeMEdge e = do
  undefined

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

-- | Returns the weight of the edge with the given source and target, if there
--   is one.
lookupEdge
  :: (Eq v, Hashable v, PrimMonad m)
  => MGraph (PrimState m) g v e
  -- ^ FIXME: doc
  -> (v, v)
  -- ^ FIXME: doc
  -> m (Maybe e)
  -- ^ FIXME: doc
lookupEdge graph (source, target) = MaybeT.runMaybeT $ do
  let edgeMap = _mgraphEdgeMap graph
  outgoingMap <- MaybeT (MHashMap.lookup edgeMap     source)
  weight      <- MaybeT (MHashMap.lookup outgoingMap target)
  pure weight

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
