--------------------------------------------------------------------------------

{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------

module EqualitySaturation where

--------------------------------------------------------------------------------

import           Control.Exception (SomeException, throwIO)
import           Control.Monad     ((>=>))
import           Data.List         (sortBy)
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.Ord          (comparing)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Data.Vector       (Vector)
import qualified Data.Vector       as Vector
import           Data.Void         (Void)
import qualified GHC.TypeLits      as TE

--------------------------------------------------------------------------------

-- `fixme` is for "holes" in the program; these holes should exist even in a
-- real implementation of equality saturation; `undefined` is for places where
-- I was too lazy to actually write the code. If Haskell had better support for
-- parameterized modules (module functors), I would just use those.

fixme :: any
fixme = error "FIXME: this value should be defined by the user of this library!"

data FIXMEType
-- #define FIXMEType (TE.TypeError (TE.Text "FIXME: this type should be defined by the user of this library!"))

--------------------------------------------------------------------------------

-- These are abstract types that we are postulating; the aforementioned "holes"
-- in this library.

-- A type of variables.
-- This could be a string or an integer or something else entirely, as long as
-- it is orderable / comparable for equality.
data Variable = Variable FIXMEType

-- A type of expressions in your referentially transparent AST.
data Expr = Expr FIXMEType

-- A type of vertex labels in a program expression graph. For a referentially
-- transparent language, this will generally be something roughly like the type
-- of AST nodes (`Expr`), except without any recursion, since that is included
-- via the edges of the graph.
--
-- For example, if you have a simple arithmetic language:
--
--     data TrigType = Sin | Cos | Tan | Sec | Csc | Cot
--     data Expr = Number Double        -- numeric literals
--               | Negate Expr          -- unary negation
--               | Plus   [Expr]        -- variadic plus
--               | Times  Expr Expr     -- binary multiplication
--               | Trig   TrigType Expr -- unary trigonometric functions
--
-- then the definition of `PEGNode` would look like:
--
--     data PEGNode = PEGNumber Double
--                  | PEGNegate
--                  | PEGPlus
--                  | PEGTimes
--                  | PEGTrig TrigType
--
-- Note that this means that the graph may not be
--
-- Also, a convenient way to define `PEGNode` is by defining `Expr` using open
-- value recursion (i.e.: by taking the fixed point of a functor `Expr'`) and
-- then defining `PEGNode = Expr' ()`. For example:
--
--     newtype Fix f = Fix { unFix :: f (Fix f) }
--     data Expr' e = Number Double | Negate e | Plus [e] | Times e e
--     type Expr = Fix Expr'
--     type PEGNode = Expr' ()
--
-- Interestingly, when you use this approach with an AST that has variadic
-- nodes (e.g.: `Plus`), the resulting `PEGNode` will contain a value of
-- type `[()]` that is essentially a unary number encoding the number of
-- children that node has. This complicates some things but simplifies others,
-- so you will have to decide whether it makes sense for your AST.
data PEGNode = PEGNode FIXMEType

--------------------------------------------------------------------------------

-- Here we postulate some values and typeclass instances on our abstract types.

-- `Variable`s must be testable for equality
instance Eq Variable where
  (==) = fixme

-- `Variable`s must be orderable (so they can be put in `Set`s)
instance Ord Variable where
  compare = fixme

-- `PEGNode`s must be testable for equality
instance Eq PEGNode where
  (==) = fixme

-- `PEGNode`s must be orderable (so they can be put in `Set`s)
instance Ord PEGNode where
  compare = fixme

-- Convert an `Expr` to a `ClosedTerm` (defined later) by converting each AST
-- node to the associated `PEGNode` constructor and then recursing on the
-- children.
exprToTerm :: Expr -> ClosedTerm
exprToTerm = fixme

-- Parse a `ClosedTerm` (defined later) back into an `Expr`.
-- This function should essentially just recurse over the tree, converting the
-- `PEGNode` into its associated `Expr` constructor and ensuring that the
-- number of children is appropriate to the constructor.
--
-- Laws:
--   * For any `e ∈ Expr`, `termToExpr (exprToTerm e) ≡ Right e`.
termToExpr :: ClosedTerm -> Either SomeException Expr
termToExpr = fixme

--------------------------------------------------------------------------------

-- I didn't feel like bringing in a real graph library, so I'm postulating one
-- with the interface defined below. A real implementation of equality
-- saturation would not be need to be polymorphic over the graph it uses, since
-- that's just an implementation detail, so I'm not using `FIXME` or `fixme`
-- in this code.

-- A type of directed graphs with vertex labels of type `v` and edge labels of
-- type `e`. Two nodes of the same label can exist, but there can only ever be
-- at most one edge with a given `(source, edge-label, target)`.
--
-- A `Graph` can be empty (i.e.: it is not a rooted graph).
data Graph v e

-- A type of pointers to graph nodes. The runtime data of a `GraphNode` should
-- also include a pointer to the whole graph, otherwise some of the functions
-- below will not be definable.
data GraphNode v e

-- A `GraphNode` is just a pair of pointers, so we should be able to compare it
-- for equality even if the vertex and edge label types cannot be compared for
-- equality. Assume for the sake of expediency that we will never compare two
-- `GraphNode`s from different graphs for equality, though you could use unsafe
-- pointer equality to implement that correctly in Haskell.
instance Eq (GraphNode v e)
instance Ord (GraphNode v e)

-- The graph with no vertices or edges.
emptyGraph :: Graph v e
emptyGraph = undefined

-- Given a graph and a vertex label, add a fresh node to the graph with that
-- label and return a reference to it.
addNode :: Graph v e -> v -> GraphNode v e
addNode = undefined

-- Given a reference to a graph node `x`, an edge label `l`, and a reference
-- to a node `y` in the same graph, add an edge from `x` to `y` with label `l`.
-- If such an edge already exists, return `Nothing`. Otherwise, return a pair
-- of references to `x` and `y` in the updated graph.
addEdge :: GraphNode v e -> e -> GraphNode v e
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
data Term var
  = MkVarTerm var
  | MkNodeTerm PEGNode (Vector (Term var))
  deriving (Eq, Ord)

-- An open term may have variables.
--
-- When I say "variable", I don't mean variables in the actual AST;
-- these are more like metasyntactic variables that may stand for any term.
type OpenTerm = Term Variable

-- A closed term is one without any variables.
type ClosedTerm = Term Void

-- Helper function to get the `Set` of free variables in the given `Term`.
freeVars :: (Ord var) => Term var -> Set var
freeVars (MkVarTerm var)         = Set.singleton var
freeVars (MkNodeTerm _ children) = Set.unions (Vector.toList
                                               (Vector.map freeVars children))

--------------------------------------------------------------------------------

-- The `UnsafeMkEquation` constructor should never be used; instead all code
-- should be written in terms of the `makeEquation` and `fromEquation`
-- functions below.
data Equation
  = UnsafeMkEquation (OpenTerm, OpenTerm, Set Variable)
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
makeEquation :: (OpenTerm, OpenTerm) -> Maybe Equation
makeEquation (rhs, lhs) = if freeRHS `Set.isSubsetOf` freeLHS
                          then Just (UnsafeMkEquation (lhs, rhs, freeLHS))
                          else Nothing
  where
    (freeLHS, freeRHS) = (freeVars lhs, freeVars rhs)

-- Get a pair containing the left- and right-hand sides of the given equation.
fromEquation :: Equation -> (OpenTerm, OpenTerm)
fromEquation (UnsafeMkEquation (lhs, rhs, _)) = (lhs, rhs)

-- Get the set of variables bound in this equation by the left-hand side.
equationBoundVariables :: Equation -> Set Variable
equationBoundVariables (UnsafeMkEquation (_, _, bounds)) = bounds

-- Helper functions for getting the left- and right-hand sides of an equation.
equationLHS, equationRHS :: Equation -> OpenTerm
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
type PerformanceHeuristic = PEG -> Double

--------------------------------------------------------------------------------

-- The `UnsafeMkPEG` constructor should never be used; instead all code should
-- be written in terms of the functions below.
--
-- The reason a `PEG` contains a `GraphNode` rather than a `Graph` is that it
-- is a rooted graph.
data PEG = UnsafeMkPEG (GraphNode PEGNode Int)

-- Smart constructor for PEGs.
--
-- Postconditions:
--   * the underlying graph of the returned PEG will never have two
--     edges with the same label coming out of the same node.
--   * if you sort the children of a node by their edge labels in increasing
--     order, then you will recover the order of the children of that node in
--     the original subterm.
makePEG :: ClosedTerm -> PEG
makePEG = undefined

-- Get the root node of the `Graph` underlying the given `PEG`.
pegRoot :: PEG -> GraphNode PEGNode Int
pegRoot (UnsafeMkPEG root) = root

-- Given a `PEG`, return a `Vector` of `PEG`s, each representing the subgraph
-- rooted at each child of the root node of the given `PEG`.
pegChildren :: PEG -> Vector PEG
pegChildren node = let outgoing :: [(Int, GraphNode PEGNode Int)]
                       outgoing = Set.toList (outgoingEdges (pegRoot node))
                       children :: Vector (GraphNode PEGNode Int)
                       children = Vector.fromList
                                  (map snd (sortBy (comparing fst) outgoing))
                   in Vector.map UnsafeMkPEG children

-- Convert a `PEG` into a term by starting at the root node and recursively
-- expanding nodes. If there is a cycle in the PEG, this will not terminate.
pegToTerm :: PEG -> ClosedTerm
pegToTerm peg = MkNodeTerm (nodeLabel (pegRoot peg))
                (Vector.map pegToTerm (pegChildren peg))

-- Modify a PEG, returning `Nothing` if the modification you made to the
-- underlying `Graph` made the PEG no longer valid (e.g.: you added two edges
-- out of the same node with the edge labels).
modifyPEG :: (Graph PEGNode Int -> Graph PEGNode Int) -> PEG -> Maybe PEG
modifyPEG = undefined

--------------------------------------------------------------------------------

-- An EPEG (or equality-PEG) is a PEG along with an equivalence relation on the
data EPEG
  = MkEPEG
    { epegPEG        :: PEG
    , epegEquivalent :: GraphNode PEGNode Int -> GraphNode PEGNode Int -> Bool
    -- ^ FIXME: replace with union-find or something
    }

-- Given a pair of
epegAddEquivalence :: (GraphNode PEGNode Int, GraphNode PEGNode Int)
                   -> EPEG -> Maybe EPEG
epegAddEquivalence (a, b) epeg
  = if epegEquivalent epeg a b
    then Nothing
    else Just (MkEPEG { epegPEG        = epegPEG epeg
                      , epegEquivalent = undefined
                      })

-- Convert a PEG into the trivial EPEG that holds every node to be semantically
-- distinct.
pegToEPEG :: PEG -> EPEG
pegToEPEG peg = MkEPEG peg (\_ _ -> False)

-- Given a set of equations and an EPEG, this will return a new EPEG that is the
-- result of matching and applying one of the equations to the EPEG. If there is
-- no place in the EPEG where any of the equations apply (and where the result
-- of applying the equation is something that is not already in the graph), then
-- this function will return `Nothing`.
saturateStep :: Set Equation -> EPEG -> Maybe EPEG
saturateStep = undefined

-- Given a performance heuristic and an EPEG, return the PEG subgraph that
-- maximizes the heuristic.
selectBest :: PerformanceHeuristic -> EPEG -> PEG
selectBest = undefined

-- The internal version of equality saturation.
saturate
  :: Set Equation
  -> PerformanceHeuristic
  -> EPEG
  -> (EPEG -> IO Bool)
  -> (PEG -> IO (Maybe a))
  -> IO [a]
saturate = undefined

-- The public interface of equality saturation.
equalitySaturation
  :: Set Equation
  -- ^ A set of optimization axioms.
  -> PerformanceHeuristic
  -- ^ The performance heuristic to optimize.
  -> Expr
  -- ^ The code whose performance will be optimized.
  -> (EPEG -> IO Bool)
  -- ^ A callback that, given the current state of the `EPEG`, will decide
  --   whether we should run `selectBest` again. In many cases, this will be
  --   some kind of timer.
  -> (Expr -> IO (Maybe a))
  -- ^ A callback that will be called with the optimized `Expr` every time
  --   `selectBest` has found a new best version of the original program.
  --   The argument is the new best version, and the return value will be
  --   collected in a list during the equality saturation loop. If `Nothing`
  --   is ever returned by this callback, equality saturation will terminate
  --   early; otherwise it will run for an amount of time that is exponential
  --   in the size of the original program.
  -> IO [a]
  -- ^ The list of results produced by the second callback, in _reverse_
  --   chronological order (e.g.: starting with newest and ending with oldest).
equalitySaturation equations heuristic initial timer callback
  = saturate
    equations heuristic (exprToEPEG initial) timer (pegToExpr >=> callback)
  where
    exprToEPEG :: Expr -> EPEG
    exprToEPEG = pegToEPEG . makePEG . exprToTerm
    pegToExpr :: PEG -> IO Expr
    pegToExpr peg = case termToExpr (pegToTerm peg) of
                      Left  exception -> throwIO exception
                      Right result    -> pure result

--------------------------------------------------------------------------------
