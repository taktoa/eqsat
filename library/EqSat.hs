--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat
  ( module EqSat -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Exception
                 (Exception, SomeException, catch, throw, throwIO)

import           Control.Applicative       (empty)

import           Control.Monad

import           Control.Monad.Primitive
import           Control.Monad.ST          (ST, runST)

import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Class as MonadTrans
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Maybe as MaybeT

import           Control.Monad.Except      (MonadError (throwError))

import           Data.Hashable             (Hashable)

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap

import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HashSet

import qualified Data.Graph.Immutable      as Graph
import qualified Data.Graph.Mutable        as MGraph
import           Data.Graph.Types          (Graph, MGraph, Vertex)
import qualified Data.Graph.Types          as Graph
import qualified Data.Graph.Types          as MGraph
import qualified Data.Graph.Types.Internal as Graph.Internal

import           Data.STRef                (STRef)
import qualified Data.STRef                as STRef

import           Data.Maybe
import           Data.Ord                  (comparing)

import           Data.Foldable             (asum)
import           Data.List                 (sortBy)

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map

import           Data.Set                  (Set)
import qualified Data.Set                  as Set

import           Data.Word                 (Word32)

import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector

import           Data.Void                 (Void, absurd)

import           Data.SBV
                 (SInteger, Symbolic, (.<), (.<=), (.==))
import qualified Data.SBV                  as SBV
import qualified Data.SBV.Internals        as SBV.Internals

import           Flow                      ((.>), (|>))

import           GHC.Generics              (Generic)

import           EqSat.MHashMap            (MHashMap)
import qualified EqSat.MHashMap            as MHashMap

import           EqSat.MHashSet            (MHashSet)
import qualified EqSat.MHashSet            as MHashSet

import           EqSat.Variable            (Variable)
import qualified EqSat.Variable            as Variable

import           EqSat.Term
                 (ClosedTerm, OpenTerm, Term (MkNodeTerm, MkVarTerm))
import qualified EqSat.Term                as Term

import           EqSat.Equation            (Equation)
import qualified EqSat.Equation            as Equation

import           EqSat.IsExpression
                 (IsExpression (exprToTerm, termToExpr))

--------------------------------------------------------------------------------

-- The `UnsafeMkPEG` constructor should never be used; instead all code should
-- be written in terms of the functions below.
--
-- The reason a `PEG` contains a `GraphNode` rather than a `Graph` is that it
-- is a rooted graph.
data PEG g node
  = UnsafeMkPEG
    !(Graph g Int node)
    !(Vertex g)
  deriving ()

-- | FIXME: doc
data SomePEG node where
  -- | FIXME: doc
  MkSomePEG :: !(PEG g node) -> SomePEG node

-- | FIXME: doc
withSomePEG
  :: SomePEG node
  -- ^ FIXME: doc
  -> (forall g. PEG g node -> result)
  -- ^ FIXME: doc
  -> result
  -- ^ FIXME: doc
withSomePEG (MkSomePEG peg) f = f peg

-- Smart constructor for PEGs.
--
-- Postconditions:
--   * the underlying graph of the returned PEG will never have two
--     edges with the same label coming out of the same node.
--   * if you sort the children of a node by their edge labels in increasing
--     order, then you will recover the order of the children of that node in
--     the original subterm.
makePEG :: Term node (SomePEG node) -> PEG g node
makePEG = undefined

-- | FIXME: doc
makePEG' :: ClosedTerm node -> PEG g node
makePEG' = fmap absurd .> makePEG

-- Get the root node of the `Graph` underlying the given `PEG`.
pegRoot :: PEG g node -> Vertex g
pegRoot (UnsafeMkPEG _ root) = root

pegGraph :: PEG g node -> Graph g Int node
pegGraph (UnsafeMkPEG graph _) = graph

pegRootNode :: PEG g node -> node
pegRootNode peg = Graph.atVertex (pegRoot peg) (pegGraph peg)

-- Given a `PEG`, return a `Vector` of `PEG`s, each representing the subgraph
-- rooted at each child of the root node of the given `PEG`.
pegChildren :: PEG g node -> Vector (PEG g node)
pegChildren = undefined -- FIXME
-- pegChildren node = let outgoing :: [(Int, GraphNode node Int)]
--                        outgoing = Set.toList (outgoingEdges (pegRoot node))
--                        children :: Vector (GraphNode node Int)
--                        children = Vector.fromList
--                                   (map snd (sortBy (comparing fst) outgoing))
--                    in Vector.map UnsafeMkPEG children

-- Convert a `PEG` into a term by starting at the root node and recursively
-- expanding nodes. If there is a cycle in the PEG, this will not terminate.
pegToTerm :: PEG g node -> ClosedTerm node
pegToTerm peg = MkNodeTerm (Graph.atVertex (pegRoot peg) (pegGraph peg))
                (Vector.map pegToTerm (pegChildren peg))

-- Modify a PEG, returning `Nothing` if the modification you made to the
-- underlying `Graph` made the PEG no longer valid (e.g.: you added two edges
-- out of the same node with the edge labels).
modifyPEG
  :: (Monad m)
  => PEG g node
  -> (Graph g Int node -> MaybeT m (Graph g' Int node))
  -> MaybeT m (PEG g' node)
modifyPEG peg f = do
  undefined

normalizePEG
  :: PEG g node
  -> (Vertex g -> Vertex g, PEG g node)
normalizePEG input = runST $ do
  updaterHM <- MHashMap.new
  pegRef <- STRef.newSTRef input
  undefined
  updater <- undefined
  output  <- STRef.readSTRef pegRef
  pure (updater, output)

traversePEG
  :: (Monad m)
  => PEG g nodeA
  -> (Vertex g -> nodeA -> m nodeB)
  -> m (PEG g nodeB)
traversePEG = undefined

--------------------------------------------------------------------------------

-- | An EPEG (or equality-PEG) is a PEG along with an equivalence relation on
--   the PEG nodes.
data EPEG g node
  = MkEPEG
    { epegPEG        :: !(PEG g node)
    , epegEqRelation :: !Int -- FIXME
    -- ^ FIXME: replace with union-find or something
    }
  deriving ()

-- | FIXME: doc
data SomeEPEG node where
  -- | FIXME: doc
  MkSomeEPEG :: !(EPEG g node) -> SomeEPEG node

-- | FIXME: doc
withSomeEPEG
  :: SomeEPEG node
  -- ^ FIXME: doc
  -> (forall g. EPEG g node -> result)
  -- ^ FIXME: doc
  -> result
  -- ^ FIXME: doc
withSomeEPEG (MkSomeEPEG epeg) f = f epeg

-- | Return a 'Bool' representing whether the two given vertices are in the same
--   class of the equivalence relation contained in the given 'EPEG'.
epegEquivalent
  :: EPEG g node
  -- ^ FIXME: doc
  -> (Vertex g, Vertex g)
  -- ^ FIXME: doc
  -> Bool
  -- ^ FIXME: doc
epegEquivalent = undefined

-- | Given a pair of vertices in an 'EPEG', combine their equivalence classes.
epegAddEquivalence
  :: (Vertex g, Vertex g)
  -> EPEG g node -> Maybe (EPEG g node)
epegAddEquivalence (a, b) epeg
  = if epegEquivalent epeg (a, b)
    then Nothing
    else Just (MkEPEG { epegPEG        = epegPEG epeg
                      , epegEqRelation = undefined (epegEqRelation epeg)
                      })

-- Convert a PEG into the trivial EPEG that holds every node to be semantically
-- distinct.
pegToEPEG :: PEG g node -> EPEG g node
pegToEPEG peg = MkEPEG peg undefined -- FIXME

epegChildren :: EPEG g node -> Vector (EPEG g node)
epegChildren (MkEPEG peg eq) = (\p -> MkEPEG p eq) <$> pegChildren peg

epegRootNode :: EPEG g node -> node
epegRootNode (MkEPEG peg _) = pegRootNode peg

epegGetClass :: EPEG g node -> Vertex g -> Maybe (Set (Vertex g))
epegGetClass = undefined

epegClasses :: EPEG g node -> Set (Set (Vertex g))
epegClasses = undefined

--------------------------------------------------------------------------------

-- The type of global performance heuristics.
--
-- In a real implementation of equality saturation, this type will not be
-- defined this way, as the efficiency of equality saturation can be improved
-- if the pseudoboolean integer solver can introspect on the way in which the
-- performance heuristic is defined, rather than treating it as an oracle.
--
-- If we were using the `sbv` library, for example, it would be reasonable
-- for `PerformanceHeuristic` to be defined as `EPEG -> Symbolic SInteger`.
type PerformanceHeuristic node
  = forall g. EPEG g (node, SBV.SBool) -> Symbolic SInteger

runPerformanceHeuristic
  :: forall g node m.
     (MonadIO m)
  => EPEG g node
  -> PerformanceHeuristic node
  -> m (Maybe (SomePEG node))
runPerformanceHeuristic epeg heuristic = MaybeT.runMaybeT $ do
  let classesSet :: Set (Set (Vertex g))
      classesSet = epegClasses epeg

  let classes :: Vector (Int, Vector (Vertex g))
      classes = Set.toList classesSet
                |> map (Set.toList .> Vector.fromList)
                |> zip [0..]
                |> Vector.fromList

  optimizeResult <- liftIO $ SBV.optimize SBV.Lexicographic $ do
    predicates <- mconcat . Vector.toList <$> do
      Vector.forM classes $ \(i, cls) -> do
        let n = Vector.length cls
        var <- SBV.sWord32 (show i)
        -- SBV.constrain (0 .<= var)
        SBV.constrain (var .< fromIntegral n)
        let vec = Vector.fromList (zip [0..] (Vector.toList cls))
        Vector.forM vec $ \(i, vertex) -> do
          pure (vertex, var .== fromIntegral i)
    let predMap = HashMap.fromList (Vector.toList predicates)
    peg <- traversePEG (epegPEG epeg) $ \vertex node -> do
      case HashMap.lookup vertex predMap of
        Just b  -> pure (node, b)
        Nothing -> error "this should never happen"
    goal <- heuristic (MkEPEG peg (epegEqRelation epeg))
    SBV.maximize "heuristic" goal

  (SBV.LexicographicResult result) <- pure optimizeResult

  let getValue :: Int -> MaybeT m Word32
      getValue i = SBV.getModelValue (show i) result |> pure |> MaybeT

  vertices <- Vector.forM classes $ \(i, cls) -> do
    value <- getValue i -- default?
    Vector.indexM cls (fromIntegral value)

  let vertexSet = HashSet.fromList (Vector.toList vertices)

  let keepVertex = HashSet.member vertexSet

  rootClass <- MaybeT (pure (epegGetClass epeg (pegRoot (epegPEG epeg))))

  root <- Vector.filter (`Set.member` rootClass) vertices
          |> Vector.toList
          |> \case [x] -> pure x
                   _   -> empty

  representativeMap <- pure $ runST $ do
    hm <- MHashMap.new @_ @(Vertex g) @(Vertex g)
    undefined
    -- Vector.forM_
    MHashMap.freeze hm

  modifyPEG (epegPEG epeg) $ \graph -> do
    -- pure $ flip Graph.mapVertices graph $ \vertex label -> do
      -- fmap Graph.Internal.Graph $ Graph.create $ \mgraph -> do
      --   undefined
      -- -- case graph of
      -- --    MkSomePEG (UnsafeMkPEG)
      -- undefined
    undefined
  -- let convertModel :: SMTModel
  -- case result of
  --   SBV.Satisfiable
  undefined

--------------------------------------------------------------------------------

matchPattern
  :: forall node var g.
     (Eq node, Ord var, Hashable var)
  => Term node var
  -> EPEG g node
  -> Maybe (HashMap var (EPEG g node))
matchPattern = do
  let go :: forall s.
            MHashMap s var (EPEG g node)
         -> Term node var
         -> EPEG g node
         -> MaybeT (ST s) ()
      go hm term epeg
        = case term of
            (MkVarTerm var) -> do
              -- This is the equivalence relation under which non-linear pattern
              -- matches are checked. Currently it checks that the two nodes are
              -- exactly equal, so this means that matching will sometimes fail
              -- if the graph does not have maximal sharing.
              let equiv :: EPEG g node -> EPEG g node -> Bool
                  equiv a b = pegRoot (epegPEG a) == pegRoot (epegPEG b)
              MHashMap.insertWith hm var epeg
                $ \a b -> guard (a `equiv` b) >> pure a
            (MkNodeTerm node children) -> do
              let pairs = Vector.zip children (epegChildren epeg)
              guard (node == epegRootNode epeg)
              Vector.forM_ pairs $ \(subpat, subgraph) -> do
                go hm subpat subgraph
  \term epeg -> runST $ MaybeT.runMaybeT $ do
    hm <- MHashMap.new
    go hm term epeg
    MHashMap.freeze hm

applyRule
  :: forall node var g.
     (Eq node, Ord var, Hashable var)
  => (Term node var, Term node var)
  -> EPEG g node
  -> Maybe (EPEG g node)
applyRule (pat, rep) epeg = runST $ MaybeT.runMaybeT $ do
  -- peg <- epegPEG epeg
  undefined

--------------------------------------------------------------------------------

-- Given a performance heuristic and an EPEG, return the PEG subgraph that
-- maximizes the heuristic.
selectBest
  :: PerformanceHeuristic node
  -> EPEG g node
  -> PEG g node
selectBest heuristic epeg = undefined

-- Given a set of equations and an EPEG, this will return a new EPEG that is the
-- result of matching and applying one of the equations to the EPEG. If there is
-- no place in the EPEG where any of the equations apply (and where the result
-- of applying the equation is something that is not already in the graph), then
-- this function will return `Nothing`.
saturateStep
  :: Set (Equation node Variable)
  -> EPEG g node
  -> Maybe (EPEG g node)
saturateStep eqs epeg = do
  undefined

-- | The internal version of equality saturation.
saturate
  :: (Monad m)
  => Set (Equation node Variable)
  -> PerformanceHeuristic node
  -> EPEG g node
  -> (EPEG g node -> m Bool)
  -> (PEG g node -> m (Maybe a))
  -> m [a]
saturate eqs heuristic initial timer cb = do
  let go epeg soFar = do
        case saturateStep eqs epeg of
          Just epeg' -> do let recurse = go epeg'
                           shouldSelectBest <- timer epeg'
                           if shouldSelectBest
                             then cb (selectBest heuristic epeg')
                                  >>= \case (Just x) -> recurse (x : soFar)
                                            Nothing  -> recurse soFar
                             else recurse soFar
          Nothing    -> pure soFar
  go initial []

-- | The public interface of equality saturation.
equalitySaturation
  :: forall node expr m a.
     (IsExpression node expr, MonadError SomeException m)
  => Set (Equation node Variable)
  -- ^ A set of optimization axioms.
  -> PerformanceHeuristic node
  -- ^ The performance heuristic to optimize.
  -> expr
  -- ^ The code whose performance will be optimized.
  -> (forall g. EPEG g node -> m Bool)
  -- ^ A callback that, given the current state of the `EPEG`, will decide
  --   whether we should run `selectBest` again. In many cases, this will be
  --   some kind of timer.
  -> (expr -> m (Maybe a))
  -- ^ A callback that will be called with the optimized `Term` every time
  --   `selectBest` has found a new best version of the original program.
  --   The argument is the new best version, and the return value will be
  --   collected in a list during the equality saturation loop. If `Nothing`
  --   is ever returned by this callback, equality saturation will terminate
  --   early; otherwise it will run for an amount of time that is exponential
  --   in the size of the original program.
  -> m [a]
  -- ^ The list of results produced by the second callback, in _reverse_
  --   chronological order (e.g.: starting with newest and ending with oldest).
equalitySaturation eqs heuristic initial timer cb
  = let exprToEPEG = exprToTerm .> makePEG' .> pegToEPEG
        pegToExpr peg = case termToExpr (pegToTerm peg) of
                          Left  exception -> throwError exception
                          Right result    -> pure result
    in saturate eqs heuristic (exprToEPEG initial) timer (pegToExpr >=> cb)

--------------------------------------------------------------------------------
