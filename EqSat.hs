--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

--------------------------------------------------------------------------------

module EqualitySaturation where

--------------------------------------------------------------------------------

import           Control.Exception         (SomeException, throwIO)

import           Control.Monad

import           Control.Monad.Primitive

import qualified Data.Graph.Immutable      as Graph
import qualified Data.Graph.Mutable        as MGraph
import           Data.Graph.Types          (Graph, MGraph, Vertex)
import qualified Data.Graph.Types          as Graph
import qualified Data.Graph.Types          as MGraph
import qualified Data.Graph.Types.Internal as Graph.Internal

import           Data.Ord                  (comparing)

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

--------------------------------------------------------------------------------

newtype Variable
  = MkVariable { variableID :: Int }
  deriving (Eq, Ord)

--------------------------------------------------------------------------------

-- | A type of term trees. Each node in the tree contains a `PEGNode` and has an
--   arbitrary number of children. This type is polymorphic over the type of
--   variables to exploit a trick that allows us to use the same type for terms
--   with and without metasyntactic variables.
data Term node var
  = MkVarTerm var
  | MkNodeTerm node (Vector (Term node var))
  deriving (Eq, Ord)

-- | An open term may have variables.
--
--   When I say "variable", I don't mean variables in the actual AST;
--   these are more like metasyntactic variables that may stand for any term.
type OpenTerm node = Term node Variable

-- | A closed term is one without any variables.
type ClosedTerm node = Term node Void

makeNodeOpenTerm
  :: (Ord var)
  => node
  -> Vector (Term node var)
  -> Maybe (Term node var)
makeNodeOpenTerm node subterms = do
  let vars = Vector.map freeVars subterms
  let sum1 = Vector.sum (Vector.map Set.size vars)
  let sum2 = Set.size (Set.unions (Vector.toList vars))
  guard (sum1 == sum2)
  pure (MkNodeTerm node subterms)

-- | Helper function to get the `Set` of free variables in the given `Term`.
freeVars :: (Ord var) => Term node var -> Set var
freeVars (MkVarTerm var)         = Set.singleton var
freeVars (MkNodeTerm _ children) = Vector.map freeVars children
                                   |> Vector.toList |> Set.unions

--------------------------------------------------------------------------------

class Expression node expr | expr -> node where
  exprToTerm :: expr -> ClosedTerm node
  termToExpr :: ClosedTerm node -> Either SomeException expr

--------------------------------------------------------------------------------

-- The `UnsafeMkEquation` constructor should never be used; instead all code
-- should be written in terms of the `makeEquation` and `fromEquation`
-- functions below.
data Equation node
  = UnsafeMkEquation (OpenTerm node, OpenTerm node, Set Variable)
  deriving (Eq, Ord)

-- Smart constructor for `Equation`s.
--
-- Laws:
--   * For any `(lhs, rhs) ∈ (OpenTerm, OpenTerm)`,
--     if `Set.isSubsetOf (freeVars rhs) (freeVars lhs) ≡ True`,
--     then `fromEquation <$> makeEquation (lhs, rhs) ≡ Just (lhs, rhs)`.
--   * For any `(lhs, rhs) ∈ (OpenTerm, OpenTerm)`,
--     if `Set.isSubsetOf (freeVars rhs) (freeVars lhs) ≡ False`,
--     then `makeEquation (lhs, rhs) ≡ Nothing`.
makeEquation :: (OpenTerm node, OpenTerm node) -> Maybe (Equation node)
makeEquation (rhs, lhs) = if freeRHS `Set.isSubsetOf` freeLHS
                          then Just (UnsafeMkEquation (lhs, rhs, freeLHS))
                          else Nothing
  where
    (freeLHS, freeRHS) = (freeVars lhs, freeVars rhs)

-- Get a pair containing the left- and right-hand sides of the given equation.
fromEquation :: Equation node -> (OpenTerm node, OpenTerm node)
fromEquation (UnsafeMkEquation (lhs, rhs, _)) = (lhs, rhs)

-- Get the set of variables bound in this equation by the left-hand side.
equationBoundVariables :: Equation node -> Set Variable
equationBoundVariables (UnsafeMkEquation (_, _, bounds)) = bounds

-- Helper functions for getting the left- and right-hand sides of an equation.
equationLHS, equationRHS :: Equation node -> OpenTerm node
equationLHS = fst . fromEquation
equationRHS = snd . fromEquation

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
type PerformanceHeuristic g node
  = EPEG g node -> Symbolic SInteger

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

matchPattern :: (Ord var)
             => Term node var
             -> EPEG g node
             -> Map var (EPEG g node)
matchPattern (MkVarTerm var) epeg = Map.singleton var epeg
matchPattern (MkNodeTerm node children) epeg = undefined

-- Given a set of equations and an EPEG, this will return a new EPEG that is the
-- result of matching and applying one of the equations to the EPEG. If there is
-- no place in the EPEG where any of the equations apply (and where the result
-- of applying the equation is something that is not already in the graph), then
-- this function will return `Nothing`.
saturateStep :: Set (Equation node) -> EPEG g node -> Maybe (EPEG g node)
saturateStep = undefined

-- Given a performance heuristic and an EPEG, return the PEG subgraph that
-- maximizes the heuristic.
selectBest :: PerformanceHeuristic g node -> EPEG g node -> PEG g node
selectBest = undefined

-- The internal version of equality saturation.
saturate
  :: Set (Equation node)
  -> PerformanceHeuristic g node
  -> EPEG g node
  -> (EPEG g node -> IO Bool)
  -> (PEG g node -> IO (Maybe a))
  -> IO [a]
saturate = undefined

-- The public interface of equality saturation.
equalitySaturation
  :: forall node expr a.
     (Expression node expr)
  => Set (Equation node)
  -- ^ A set of optimization axioms.
  -> (forall g. PerformanceHeuristic g node)
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
