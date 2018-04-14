--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

--------------------------------------------------------------------------------

module EqualitySaturation where

--------------------------------------------------------------------------------

import           Control.Exception       (SomeException, throwIO)

import           Control.Monad           ((>=>))

import           Control.Monad.Primitive

import           Data.List               (sortBy)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Ord                (comparing)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector
import           Data.Void               (Void)

import           Data.SBV                (SInteger, Symbolic)
import qualified Data.SBV                as SBV

import           Flow                    ((.>), (|>))

--------------------------------------------------------------------------------

fixme :: any
fixme = error "FIXME: this value should be defined by the user of this library!"

data FIXMEType

--------------------------------------------------------------------------------

newtype Variable
  = MkVariable { variableID :: Int }
  deriving (Eq, Ord)

--------------------------------------------------------------------------------

-- | A type of directed graphs with vertex labels of type `v` and edge labels of
--   type `e`. Two nodes of the same label can exist, but there can only ever be
--   at most one edge with a given `(source, edge-label, target)`.
--
--   A `Graph` can be empty (i.e.: it is not a rooted graph).
data Graph s g v e

-- | A type of pointers to graph nodes. The runtime data of a `GraphNode` should
--   also include a pointer to the whole graph, otherwise some of the functions
--   below will not be definable.
data GraphNode s g v e

-- | A `GraphNode` is just a pair of pointers, so we should be able to compare
--   it for equality even if the vertex and edge label types cannot be compared
--   for equality.
instance Eq (GraphNode s g v e)
instance Ord (GraphNode s g v e)

-- | Create a graph with no vertices or edges.
emptyGraph
  :: (PrimMonad m)
  => m (Graph (PrimState m) g v e)
emptyGraph = undefined

-- | Return all the nodes in the given graph, in the order they were added.
graphNodes
  :: (PrimMonad m)
  => Graph s g v e
  -> Vector (GraphNode (PrimState m) g v e)
graphNodes = undefined

-- | Given a graph and a vertex label, add a fresh node to the graph with that
--   label and return a reference to it.
addNode
  :: (PrimMonad m)
  => Graph s g v e
  -> v
  -> GraphNode g v e
addNode = undefined

-- | Given a reference to a graph node @x@, an edge label @l@, and a reference
--   to a node @y@ in the same graph, add an edge from @x@ to @y@ labelled @l@.
--   If such an edge already exists, return 'Nothing'. Otherwise, return a pair
--   of references to @x@ and @y@ in the updated graph.
addEdge
  :: GraphNode v e -> e -> GraphNode v e
  -> Maybe (GraphNode v e, GraphNode v e)
addEdge = undefined

-- Given a reference to a node in a graph, returns a `Set` of pairs of node
-- references and edge labels, where each pair corresponds to an edge pointing
-- at the node. It is a `Set` rather than a list (`[…]`) because there cannot be
-- two edges with the same source node and label entering the same node.
ingoingEdges :: (Ord e) => GraphNode v e -> Set (GraphNode v e, e)
ingoingEdges = undefined

-- Given a reference to a node in a graph, returns a `Set` of pairs of edge
-- labels and node references, where each pair corresponds to an edge leaving
-- the node. It is a `Set` rather than a list (`[…]`) because there cannot be
-- two edges with the same label and target leaving the same node.
outgoingEdges :: (Ord e) => GraphNode v e -> Set (e, GraphNode v e)
outgoingEdges = undefined

-- Given a reference to a node in a graph, remove that node from the graph and
-- return the updated graph.
removeNode :: GraphNode v e -> Graph v e
removeNode = undefined

-- Given a reference to a node in a graph and an edge label, remove the edge
-- leaving that node with the given label.
--
-- If there is no such edge, return `Nothing`.
-- Otherwise, return a reference to the same node in the updated graph.
removeEdge :: GraphNode v e -> e -> Maybe (GraphNode v e)
removeEdge = undefined

-- Given a reference to a node in a graph, return the entire graph.
nodeGraph :: GraphNode v e -> Graph v e
nodeGraph = undefined

-- Given a reference to a node in a graph, return the label of that node.
nodeLabel :: GraphNode v e -> v
nodeLabel = undefined

--------------------------------------------------------------------------------

-- A type of term trees. Each node in the tree contains a `PEGNode` and has an
-- arbitrary number of children. This type is polymorphic over the type of
-- variables to exploit a trick that allows us to use the same type for terms
-- with and without metasyntactic variables.
data Term node var
  = MkVarTerm var
  | MkNodeTerm node (Vector (Term node var))
  deriving (Eq, Ord)

-- An open term may have variables.
--
-- When I say "variable", I don't mean variables in the actual AST;
-- these are more like metasyntactic variables that may stand for any term.
type OpenTerm node = Term node Variable

-- A closed term is one without any variables.
type ClosedTerm node = Term node Void

-- Helper function to get the `Set` of free variables in the given `Term`.
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
type PerformanceHeuristic node
  = EPEG node -> Symbolic SInteger

--------------------------------------------------------------------------------

-- The `UnsafeMkPEG` constructor should never be used; instead all code should
-- be written in terms of the functions below.
--
-- The reason a `PEG` contains a `GraphNode` rather than a `Graph` is that it
-- is a rooted graph.
data PEG node = UnsafeMkPEG (GraphNode node Int)

-- Smart constructor for PEGs.
--
-- Postconditions:
--   * the underlying graph of the returned PEG will never have two
--     edges with the same label coming out of the same node.
--   * if you sort the children of a node by their edge labels in increasing
--     order, then you will recover the order of the children of that node in
--     the original subterm.
makePEG :: ClosedTerm node -> PEG node
makePEG = undefined

-- Get the root node of the `Graph` underlying the given `PEG`.
pegRoot :: PEG node -> GraphNode node Int
pegRoot (UnsafeMkPEG root) = root

-- Given a `PEG`, return a `Vector` of `PEG`s, each representing the subgraph
-- rooted at each child of the root node of the given `PEG`.
pegChildren :: forall node. PEG node -> Vector (PEG node)
pegChildren node = let outgoing :: [(Int, GraphNode node Int)]
                       outgoing = Set.toList (outgoingEdges (pegRoot node))
                       children :: Vector (GraphNode node Int)
                       children = Vector.fromList
                                  (map snd (sortBy (comparing fst) outgoing))
                   in Vector.map UnsafeMkPEG children

-- Convert a `PEG` into a term by starting at the root node and recursively
-- expanding nodes. If there is a cycle in the PEG, this will not terminate.
pegToTerm :: PEG node -> ClosedTerm node
pegToTerm peg = MkNodeTerm (nodeLabel (pegRoot peg))
                (Vector.map pegToTerm (pegChildren peg))

-- Modify a PEG, returning `Nothing` if the modification you made to the
-- underlying `Graph` made the PEG no longer valid (e.g.: you added two edges
-- out of the same node with the edge labels).
modifyPEG :: (Graph node Int -> Graph node Int)
          -> PEG node -> Maybe (PEG node)
modifyPEG = undefined

--------------------------------------------------------------------------------

-- An EPEG (or equality-PEG) is a PEG along with an equivalence relation on the
data EPEG node
  = MkEPEG
    { epegPEG        :: PEG node
    , epegEquivalent :: GraphNode node Int -> GraphNode node Int -> Bool
    -- ^ FIXME: replace with union-find or something
    }

-- Given a pair of
epegAddEquivalence :: (GraphNode node Int, GraphNode node Int)
                   -> EPEG node -> Maybe (EPEG node)
epegAddEquivalence (a, b) epeg
  = if epegEquivalent epeg a b
    then Nothing
    else Just (MkEPEG { epegPEG        = epegPEG epeg
                      , epegEquivalent = undefined
                      })

-- Convert a PEG into the trivial EPEG that holds every node to be semantically
-- distinct.
pegToEPEG :: PEG node -> EPEG node
pegToEPEG peg = MkEPEG peg (\_ _ -> False)

matchPattern :: (Ord var) => Term node var -> EPEG node -> Map var (EPEG node)
matchPattern (MkVarTerm var) epeg = Map.singleton var epeg
matchPattern (MkNodeTerm node children) epeg = undefined

-- Given a set of equations and an EPEG, this will return a new EPEG that is the
-- result of matching and applying one of the equations to the EPEG. If there is
-- no place in the EPEG where any of the equations apply (and where the result
-- of applying the equation is something that is not already in the graph), then
-- this function will return `Nothing`.
saturateStep :: Set (Equation node) -> EPEG node -> Maybe (EPEG node)
saturateStep = undefined

-- Given a performance heuristic and an EPEG, return the PEG subgraph that
-- maximizes the heuristic.
selectBest :: PerformanceHeuristic node -> EPEG node -> PEG node
selectBest = undefined

-- The internal version of equality saturation.
saturate
  :: Set (Equation node)
  -> PerformanceHeuristic node
  -> EPEG node
  -> (EPEG node -> IO Bool)
  -> (PEG node -> IO (Maybe a))
  -> IO [a]
saturate = undefined

-- The public interface of equality saturation.
equalitySaturation
  :: forall node expr a.
     (Expression node expr)
  => Set (Equation node)
  -- ^ A set of optimization axioms.
  -> PerformanceHeuristic node
  -- ^ The performance heuristic to optimize.
  -> expr
  -- ^ The code whose performance will be optimized.
  -> (EPEG node -> IO Bool)
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
  = let exprToEPEG :: expr -> EPEG node
        exprToEPEG = exprToTerm .> makePEG .> pegToEPEG
        pegToExpr :: PEG node -> IO expr
        pegToExpr peg = case termToExpr (pegToTerm peg) of
                          Left  exception -> throwIO exception
                          Right result    -> pure result
    in saturate eqs heuristic (exprToEPEG initial) timer (pegToExpr >=> cb)

--------------------------------------------------------------------------------
