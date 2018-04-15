--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat
  ( module EqSat -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Exception
                 (Exception, SomeException, catch, throw, throwIO)

import           Control.Monad

import           Control.Monad.Primitive
import           Control.Monad.ST          (ST, runST)

import           Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Class as MonadTrans
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Maybe as MaybeT

import           Data.Hashable             (Hashable)

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap

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

import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector

import           Data.Void                 (Void)

import           Data.SBV                  (SInteger, Symbolic)
import qualified Data.SBV                  as SBV

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
  = forall g. EPEG g node -> Symbolic SInteger

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

-- Smart constructor for PEGs.
--
-- Postconditions:
--   * the underlying graph of the returned PEG will never have two
--     edges with the same label coming out of the same node.
--   * if you sort the children of a node by their edge labels in increasing
--     order, then you will recover the order of the children of that node in
--     the original subterm.
makePEG :: ClosedTerm node -> PEG g node
makePEG = undefined

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
modifyPEG :: (Graph g Int node -> Graph g Int node)
          -> PEG g node -> Maybe (PEG g node)
modifyPEG = undefined

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


--------------------------------------------------------------------------------

-- An EPEG (or equality-PEG) is a PEG along with an equivalence relation on the
data EPEG g node
  = MkEPEG
    { epegPEG        :: PEG g node
    , epegEqRelation :: Int -- FIXME
    -- ^ FIXME: replace with union-find or something
    }

epegEquivalent :: EPEG g node -> Vertex g -> Vertex g -> Bool
epegEquivalent = undefined

-- Given a pair of
epegAddEquivalence :: (Vertex g, Vertex g)
                   -> EPEG g node -> Maybe (EPEG g node)
epegAddEquivalence (a, b) epeg
  = if epegEquivalent epeg a b
    then Nothing
    else Just (MkEPEG { epegPEG        = epegPEG epeg
                      , epegEqRelation = undefined
                      })

-- Convert a PEG into the trivial EPEG that holds every node to be semantically
-- distinct.
pegToEPEG :: PEG g node -> EPEG g node
pegToEPEG peg = MkEPEG peg undefined -- FIXME

epegChildren :: EPEG g node -> Vector (EPEG g node)
epegChildren (MkEPEG peg eq) = (\p -> MkEPEG p eq) <$> pegChildren peg

epegRootNode :: EPEG g node -> node
epegRootNode (MkEPEG peg _) = pegRootNode peg

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

-- Given a performance heuristic and an EPEG, return the PEG subgraph that
-- maximizes the heuristic.
selectBest
  :: PerformanceHeuristic node
  -> EPEG g node
  -> PEG g node
selectBest heuristic epeg = undefined

-- | The internal version of equality saturation.
saturate
  :: Set (Equation node Variable)
  -> PerformanceHeuristic node
  -> EPEG g node
  -> (EPEG g node -> IO Bool)
  -> (PEG g node -> IO (Maybe a))
  -> IO [a]
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
  :: forall node expr a.
     (IsExpression node expr)
  => Set (Equation node Variable)
  -- ^ A set of optimization axioms.
  -> PerformanceHeuristic node
  -- ^ The performance heuristic to optimize.
  -> expr
  -- ^ The code whose performance will be optimized.
  -> (forall g. EPEG g node -> IO Bool)
  -- ^ A callback that, given the current state of the `EPEG`, will decide
  --   whether we should run `selectBest` again. In many cases, this will be
  --   some kind of timer.
  -> (expr -> IO (Maybe a))
  -- ^ A callback that will be called with the optimized `Term` every time
  --   `selectBest` has found a new best version of the original program.
  --   The argument is the new best version, and the return value will be
  --   collected in a list during the equality saturation loop. If `Nothing`
  --   is ever returned by this callback, equality saturation will terminate
  --   early; otherwise it will run for an amount of time that is exponential
  --   in the size of the original program.
  -> IO [a]
  -- ^ The list of results produced by the second callback, in _reverse_
  --   chronological order (e.g.: starting with newest and ending with oldest).
equalitySaturation eqs heuristic initial timer cb
  = let exprToEPEG :: expr -> EPEG g node
        exprToEPEG = exprToTerm .> makePEG .> pegToEPEG
        pegToExpr :: PEG g node -> IO expr
        pegToExpr peg = case termToExpr (pegToTerm peg) of
                          Left  exception -> throwIO exception
                          Right result    -> pure result
    in saturate eqs heuristic (exprToEPEG initial) timer (pegToExpr >=> cb)

--------------------------------------------------------------------------------
