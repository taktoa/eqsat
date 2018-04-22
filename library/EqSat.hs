--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat
  ( module EqSat -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Exception
                 (AssertionFailed, Exception, SomeException, assert, catch,
                 throw, throwIO)

import           Control.Applicative             (empty)

import           Control.Monad

import           Control.Monad.Primitive
import           Control.Monad.ST                (ST, runST)

import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Trans.Class       (MonadTrans (lift))
import qualified Control.Monad.Trans.Class       as MonadTrans
import           Control.Monad.Trans.Maybe       (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Maybe       as MaybeT
import           Control.Monad.Trans.Reader      (ReaderT (ReaderT))
import qualified Control.Monad.Trans.Reader      as ReaderT

import           Control.Monad.Except
                 (ExceptT, MonadError (throwError))
import qualified Control.Monad.Except            as ExceptT

import           Data.Hashable                   (Hashable)

import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS

import qualified Data.Graph.Immutable            as Graph
import qualified Data.Graph.Mutable              as MGraph
import           Data.Graph.Types
                 (Graph, MGraph, SomeGraph, Vertex)
import qualified Data.Graph.Types                as Graph
import qualified Data.Graph.Types                as MGraph
import qualified Data.Graph.Types.Internal       as Graph.Internal

import           Data.Partition                  (Partition)
import qualified Data.Partition                  as Partition

import           Data.STRef                      (STRef)
import qualified Data.STRef                      as STRef

import           Data.Maybe
import           Data.Ord                        (comparing)

import           Data.Foldable                   (asum)
import           Data.List                       (sortBy)

import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map

import           Data.Set                        (Set)
import qualified Data.Set                        as Set

import           Data.Word                       (Word16, Word32)

import           Data.Unique                     (Unique)
import qualified Data.Unique                     as Unique

import           Data.Vector                     (Vector)
import qualified Data.Vector                     as Vector

import           Data.Void                       (Void, absurd)

import           Data.Proxy                      (Proxy (Proxy))

import           Data.SBV
                 (SBV, SInteger, Symbolic, (.<), (.<=), (.==))
import qualified Data.SBV                        as SBV
import qualified Data.SBV.Internals              as SBV.Internals

import qualified EqSat.Internal.SBV

import           Flow                            ((.>), (|>))

import           GHC.Generics                    (Generic)

import           EqSat.Internal.MHashMap         (MHashMap)
import qualified EqSat.Internal.MHashMap         as MHashMap

import           EqSat.Internal.MHashSet         (MHashSet)
import qualified EqSat.Internal.MHashSet         as MHashSet

import           EqSat.Variable                  (Variable)
import qualified EqSat.Variable                  as Variable

import           EqSat.Term
                 (ClosedTerm, GTerm, OpenTerm, TTerm,
                 Term (MkNodeTerm, MkRefTerm, MkVarTerm))
import qualified EqSat.Term                      as Term

import           EqSat.TypedTerm
                 (Substitution, TypedGTerm, TypedTTerm, TypedTerm)
import qualified EqSat.TypedTerm                 as TypedTerm

import           EqSat.Equation                  (Equation)
import qualified EqSat.Equation                  as Equation

import           EqSat.Domain                    (Domain)

import           EqSat.IsExpression
                 (IsExpression (exprToGTerm, gtermToExpr))

import           EqSat.TypeSystem
                 (TypeSystem (Type, TypeError, inferType, isSubtype))
import qualified EqSat.TypeSystem                as TypeSystem

import           EqSat.Errors.CheckEquationError
                 (CheckEquationError,
                 EquationSide (EquationSideLHS, EquationSideRHS))
import qualified EqSat.Errors.CheckEquationError as CheckEquationError

import qualified EqSat.Internal.PrettyPrinter    as PP

--------------------------------------------------------------------------------

-- | FIXME: doc
data TypedEquation node var ty
  = UnsafeMkTypedEquation
    { _typedEquationUnderlying :: !(Equation node var)
    , _typedEquationType       :: !ty
    , _typedEquationVarTypes   :: !(var -> Maybe ty)
    }
  deriving ()

-- | FIXME: doc
mapError
  :: (MonadError e2 m)
  => (e1 -> e2)
  -- ^ FIXME: doc
  -> ExceptT e1 m a
  -- ^ FIXME: doc
  -> m a
  -- ^ FIXME: doc
mapError f action = do
  result <- ExceptT.runExceptT action
  case result of
    Left  e -> throwError (f e)
    Right r -> pure r

-- | FIXME: doc
checkEquation
  :: forall expr m node var.
     ( Ord var, Hashable var
     , TypeSystem node expr
     , MonadError (CheckEquationError node var expr) m
     )
  => (TTerm node var, GTerm node var)
  -- ^ FIXME: doc
  -> m (TypedEquation node var (Type expr))
  -- ^ FIXME: doc
checkEquation (lhs, rhs) = do
  let throwImpossible :: PP.Doc Void -> m a
      throwImpossible = CheckEquationError.Impossible lhs rhs .> throwError

  typedLHS <- inferType lhs
              |> (mapError
                  (CheckEquationError.InferenceFailure lhs rhs EquationSideLHS))
  typedRHS <- inferType rhs
              |> (mapError
                  (CheckEquationError.InferenceFailure lhs rhs EquationSideRHS))

  let lhsMTF  = TypedTerm.metavariableTypingFunction typedLHS
  let rhsMTF  = TypedTerm.metavariableTypingFunction typedRHS
  let lhsWTTF = TypedTerm.wholeTermTypingFunction    typedLHS
  let rhsWTTF = TypedTerm.wholeTermTypingFunction    typedRHS
  let lhsFVs  = TypedTerm.freeVars                   typedLHS
  let rhsFVs  = TypedTerm.freeVars                   typedRHS

  forM_ rhsFVs $ \usedVar -> do
    when (usedVar `Set.notMember` lhsFVs) $ do
      let err = CheckEquationError.OutOfScope
                { CheckEquationError._OutOfScope_var = usedVar
                , CheckEquationError._OutOfScope_lhs = typedLHS
                , CheckEquationError._OutOfScope_rhs = typedRHS
                }
      throwError err

    let mtfFailure = throwImpossible "metavariable typing function failure"

    lhsVarType <- maybe mtfFailure pure (lhsMTF usedVar)
    rhsVarType <- maybe mtfFailure pure (rhsMTF usedVar)

    unless (rhsVarType `isSubtype` lhsVarType) $ do
      let err = CheckEquationError.MetaVarNotSubtype
                { CheckEquationError._MetaVarNotSubtype_var        = usedVar
                , CheckEquationError._MetaVarNotSubtype_lhs        = typedLHS
                , CheckEquationError._MetaVarNotSubtype_rhs        = typedRHS
                , CheckEquationError._MetaVarNotSubtype_lhsVarType = lhsVarType
                , CheckEquationError._MetaVarNotSubtype_rhsVarType = rhsVarType
                }
      throwError err

  let wttfFailure = throwImpossible "whole-term typing function failure"

  lhsType <- maybe wttfFailure pure (lhsWTTF rhsMTF) -- not a typo
  rhsType <- maybe wttfFailure pure (rhsWTTF rhsMTF)

  unless ((lhsType `isSubtype` rhsType) && (rhsType `isSubtype` lhsType)) $ do
    let err = CheckEquationError.OverallNotEqual
              { CheckEquationError._OverallNotEqual_lhs     = typedLHS
              , CheckEquationError._OverallNotEqual_rhs     = typedRHS
              , CheckEquationError._OverallNotEqual_lhsType = lhsType
              , CheckEquationError._OverallNotEqual_rhsType = rhsType
              }
    throwError err

  equation <- Equation.make (lhs, rhs)
              |> maybe (throwImpossible "equation creation failure") pure

  let result = UnsafeMkTypedEquation
               { _typedEquationUnderlying = equation
               , _typedEquationType       = rhsType
               , _typedEquationVarTypes   = rhsMTF
               }

  pure result

--------------------------------------------------------------------------------

-- | FIXME: doc
type Edge g = (Vertex g, Vertex g)

-- | FIXME: doc
quotientGraph
  :: (Eq v, Hashable v)
  => ((Vertex g, v) -> (Vertex g, v))
  -- ^ FIXME: doc
  -> ((Edge g, e) -> (Edge g, e))
  -- ^ FIXME: doc
  -> Graph g e v
  -- ^ FIXME: doc
  -> Graph g e v
  -- ^ FIXME: doc
quotientGraph vf ef = undefined

-- | FIXME: doc
quotientSomeGraph
  :: (Eq v, Hashable v)
  => (forall g. (Vertex g, v) -> (Vertex g, v))
  -- ^ FIXME: doc
  -> (forall g. (Edge g, e) -> (Edge g, e))
  -- ^ FIXME: doc
  -> SomeGraph e v
  -- ^ FIXME: doc
  -> SomeGraph e v
  -- ^ FIXME: doc
quotientSomeGraph vf ef
  = Graph.mapSome (quotientGraph vf ef)

--------------------------------------------------------------------------------

-- | A 'PEG', or Program Expression Graph, is a rooted directed graph
--   representing a referentially transparent AST with sharing.
data PEG g node
  = UnsafeMkPEG
    { _pegGraph :: !(Graph g Int Unique)
      -- ^ The 'Graph' underlying the 'PEG'.
    , _pegNodes :: !(Vertex g -> node)
      -- ^ FIXME: doc
    , _pegRoot  :: !(Vertex g)
      -- ^ The root node of the 'PEG'.
    }
  deriving ()

-- FIXME: write instance
-- instance Eq (PEG g node) where


-- | Smart constructor for PEGs.
--
--   Postconditions:
--     * the underlying graph of the returned PEG will never have two
--       edges with the same label coming out of the same node.
--     * if you sort the children of a node by their edge labels in increasing
--       order, then you will recover the order of the children of that node in
--       the original subterm.
makePEG
  :: Term repr node (SomePEG node)
  -- ^ FIXME: doc
  -> PEG g node
  -- ^ FIXME: doc
makePEG = undefined

-- | FIXME: doc
makePEG'
  :: ClosedTerm repr node
  -- ^ FIXME: doc
  -> PEG g node
  -- ^ FIXME: doc
makePEG' = fmap absurd .> makePEG

-- | Get the root node of the 'Graph' underlying the given 'PEG'.
pegRoot
  :: PEG g node
  -- ^ FIXME: doc
  -> Vertex g
  -- ^ FIXME: doc
pegRoot (UnsafeMkPEG _ _ root) = root

-- | FIXME: doc
pegNodes
  :: PEG g node
  -- ^ FIXME: doc
  -> Vertex g
  -- ^ FIXME: doc
  -> node
  -- ^ FIXME: doc
pegNodes (UnsafeMkPEG _ f _) = f

-- | FIXME: doc
pegGraph
  :: PEG g node
  -- ^ FIXME: doc
  -> Graph g Int Unique
  -- ^ FIXME: doc
pegGraph (UnsafeMkPEG graph _ _) = graph

-- | FIXME: doc
pegAtVertex
  :: PEG g node
  -- ^ FIXME: doc
  -> Vertex g
  -- ^ FIXME: doc
  -> node
  -- ^ FIXME: doc
pegAtVertex = pegNodes
-- pegAtVertex peg vertex = snd (Graph.atVertex vertex (pegGraph peg))

-- | FIXME: doc
pegRootNode
  :: PEG g node
  -- ^ FIXME: doc
  -> node
  -- ^ FIXME: doc
pegRootNode peg = pegAtVertex peg (pegRoot peg)

-- | Given a 'PEG', return a 'Vector' of 'PEG's, each representing the subgraph
--   rooted at each child of the root node of the given 'PEG'.
pegChildren
  :: PEG g node
  -- ^ FIXME: doc
  -> Vector (PEG g node)
  -- ^ FIXME: doc
pegChildren = undefined -- FIXME
-- pegChildren node = let outgoing :: [(Int, GraphNode node Int)]
--                        outgoing = Set.toList (outgoingEdges (pegRoot node))
--                        children :: Vector (GraphNode node Int)
--                        children = Vector.fromList
--                                   (map snd (sortBy (comparing fst) outgoing))
--                    in Vector.map UnsafeMkPEG children

-- | Convert a 'PEG' into a term by starting at the root node and recursively
--   expanding nodes. If there is a cycle in the 'PEG', this will not terminate.
pegToTerm
  :: PEG g node
  -- ^ FIXME: doc
  -> ClosedTerm Term.ReprG node
  -- ^ FIXME: doc
pegToTerm peg = MkNodeTerm
                (pegRootNode peg)
                (Vector.map pegToTerm (pegChildren peg))

-- | Modify a 'PEG', returning 'Nothing' if the modification you made to the
--   underlying 'Graph' made the 'PEG' no longer valid (e.g.: you added two
--   edges out of the same node with the same edge labels).
modifyPEG
  :: (Monad m)
  => PEG g node
  -- ^ FIXME: doc
  -> (Graph g Int node -> MaybeT m (Graph g' Int node))
  -- ^ FIXME: doc
  -> MaybeT m (PEG g' node)
  -- ^ FIXME: doc
modifyPEG peg f = do
  undefined

-- | FIXME: doc
normalizePEG
  :: PEG g node
  -- ^ FIXME: doc
  -> (Vertex g -> Vertex g, PEG g node)
  -- ^ FIXME: doc
normalizePEG input = runST $ do
  updaterHM <- MHashMap.new
  pegRef <- STRef.newSTRef input
  undefined
  updater <- undefined
  output  <- STRef.readSTRef pegRef
  pure (updater, output)

-- | FIXME: doc
traversePEG
  :: (Monad m)
  => PEG g nodeA
  -- ^ FIXME: doc
  -> (Vertex g -> nodeA -> m nodeB)
  -- ^ FIXME: doc
  -> m (PEG g nodeB)
  -- ^ FIXME: doc
traversePEG = undefined

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- | An 'EPEG' (or equivalence PEG) is a 'PEG' along with an equivalence
--   relation on the 'PEG' nodes.
data EPEG g node
  = MkEPEG
    { epegPEG        :: !(PEG g node)
      -- ^ The underlying 'PEG'.
    , epegEqRelation :: !(Partition (Vertex g))
      -- ^ The equivalence relation on nodes.
    }
  deriving ()

-- | Return a 'Bool' representing whether the two given vertices are in the same
--   class of the equivalence relation contained in the given 'EPEG'.
epegEquivalent
  :: EPEG g node
  -- ^ FIXME: doc
  -> (Vertex g, Vertex g)
  -- ^ FIXME: doc
  -> Bool
  -- ^ FIXME: doc
epegEquivalent epeg (a, b)
  = let p = epegEqRelation epeg
    in Partition.rep p a == Partition.rep p b

-- | Given a pair of vertices in an 'EPEG', combine their equivalence classes.
epegAddEquivalence
  :: (Vertex g, Vertex g)
  -- ^ FIXME: doc
  -> EPEG g node
  -- ^ FIXME: doc
  -> Maybe (EPEG g node)
  -- ^ FIXME: doc
epegAddEquivalence (a, b) epeg
  = if epegEquivalent epeg (a, b)
    then Nothing
    else Just (epeg { epegEqRelation = epegEqRelation epeg
                                       |> Partition.joinElems a b
                    })

-- | Convert a 'PEG' into the trivial 'EPEG' that holds every node to be
--   semantically distinct.
pegToEPEG
  :: PEG g node
  -- ^ FIXME: doc
  -> EPEG g node
  -- ^ FIXME: doc
pegToEPEG peg = MkEPEG peg Partition.discrete

-- | FIXME: doc
epegChildren
  :: EPEG g node
  -- ^ FIXME: doc
  -> Vector (EPEG g node)
  -- ^ FIXME: doc
epegChildren (MkEPEG peg eq) = (\p -> MkEPEG p eq) <$> pegChildren peg

-- | FIXME: doc
epegRootNode
  :: EPEG g node
  -- ^ FIXME: doc
  -> node
  -- ^ FIXME: doc
epegRootNode (MkEPEG peg _) = pegRootNode peg

-- | FIXME: doc
epegGetClass
  :: EPEG g node
  -- ^ FIXME: doc
  -> Vertex g
  -- ^ FIXME: doc
  -> Maybe (Set (Vertex g))
  -- ^ FIXME: doc
epegGetClass = undefined

-- | FIXME: doc
epegClasses
  :: EPEG g node
  -- ^ FIXME: doc
  -> Set (Set (Vertex g))
  -- ^ FIXME: doc
epegClasses = undefined

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- | FIXME: doc
class Heuristic heuristic where
  data HeuristicConfig heuristic :: *

  -- | FIXME: doc
  defaultHeuristicConfig
    :: (MonadIO m)
    => proxy heuristic
    -- ^ FIXME: doc
    -> m (HeuristicConfig heuristic)
    -- ^ FIXME: doc

  -- | FIXME: doc
  runHeuristic
    :: (MonadIO m)
    => HeuristicConfig heuristic
    -- ^ FIXME: doc
    -> EPEG g node
    -- ^ FIXME: doc
    -> heuristic node
    -- ^ FIXME: doc
    -> m (Maybe (SomePEG node))
    -- ^ FIXME: doc

--------------------------------------------------------------------------------

-- | The type of symbolic performance heuristics.
data SymbolicHeuristic domain node
  = MkSymbolicHeuristic
    { _SymbolicHeuristic_valuation
      :: !(forall g. EPEG g (node, SBV Bool) -> Symbolic (SBV domain))
    -- , _SymbolicHeuristic_validation
    --   :: !(SomePEG node -> IO Bool)
    }
  deriving ()

-- | FIXME: doc
applySymbolicHeuristic
  :: SymbolicHeuristic domain node
  -- ^ FIXME: doc
  -> EPEG g (node, SBV Bool)
  -- ^ FIXME: doc
  -> Symbolic (SBV domain)
  -- ^ FIXME: doc
applySymbolicHeuristic (MkSymbolicHeuristic f) = f

-- | Optimize the given 'SymbolicHeuristic' on the
--   given 'EPEG' via pseudo-boolean integer programming using @sbv@ / @Z3@'s
--   optimization support.
--
--   If the solver terminates successfully, a 'SomePEG' representing the
--   best selected sub-'PEG' is returned.
runSymbolicHeuristic
  :: forall node domain m g.
     (MonadIO m, Domain domain)
  => SBV.SMTConfig
  -- ^ FIXME: doc
  -> EPEG g node
  -- ^ FIXME: doc
  -> SymbolicHeuristic domain node
  -- ^ FIXME: doc
  -> m (Maybe (SomePEG node))
  -- ^ FIXME: doc
runSymbolicHeuristic smtCfg epeg heuristic = MaybeT.runMaybeT $ do
  let classesSet :: Set (Set (Vertex g))
      classesSet = epegClasses epeg

  let classes :: Vector (Int, Vector (Vertex g))
      classes = Set.toList classesSet
                |> map (Set.toList .> Vector.fromList)
                |> zip [0..]
                |> Vector.fromList

  -- let isValid :: SomePEG node -> m Bool
  --     isValid = _SymbolicHeuristic_validation heuristic .> liftIO

  let goal :: () -- FIXME: validation loop
           -> Symbolic ()
      goal constraints = do
        predicates <- mconcat . Vector.toList <$> do
          Vector.forM classes $ \(i, cls) -> do
            let n = Vector.length cls
            when (toInteger n > toInteger (maxBound :: Word16)) $ do
              error "Size of equivalence class is too large!"
            var <- SBV.sWord16 (show i)
            SBV.constrain (0 .<= var)
            SBV.constrain (var .< fromIntegral n)
            let vec = zip ([0..] :: [Int]) (Vector.toList cls)
                      |> Vector.fromList
            Vector.forM vec $ \(j, vertex) -> do
              pure (vertex, var .== fromIntegral j)
        let predMap = HM.fromList (Vector.toList predicates)
        peg <- traversePEG (epegPEG epeg) $ \vertex node -> do
          case HM.lookup vertex predMap of
            Just b  -> pure (node, b)
            Nothing -> error "this should never happen"
        g <- applySymbolicHeuristic heuristic
             $ MkEPEG peg (epegEqRelation epeg)
        SBV.maximize "heuristic" g

  result <- do
    liftIO (SBV.optimizeWith smtCfg SBV.Lexicographic (goal ()))
      >>= (\case (SBV.LexicographicResult r) -> pure r
                 _                           -> fail "optimize failed")

  let getValue :: Int -> MaybeT m Word32
      getValue i = SBV.getModelValue (show i) result |> pure |> MaybeT

  vertices <- Vector.forM classes $ \(i, cls) -> do
    value <- getValue i -- default?
    Vector.indexM cls (fromIntegral value)

  let vertexSet = HS.fromList (Vector.toList vertices)

  let keepVertex = HS.member vertexSet

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

-- | FIXME: doc
instance (Domain domain) => Heuristic (SymbolicHeuristic domain) where
  data HeuristicConfig (SymbolicHeuristic domain)
    = MkSymbolicHeuristicConfig
      { _SymbolicHeuristicConfig_smtConfig :: SBV.SMTConfig
      }
  defaultHeuristicConfig _ = do
    smtConfig <- EqSat.Internal.SBV.smtConfig
    pure (MkSymbolicHeuristicConfig smtConfig)
  runHeuristic cfg epeg h = do
    let smtCfg = _SymbolicHeuristicConfig_smtConfig cfg
    runSymbolicHeuristic smtCfg epeg h

--------------------------------------------------------------------------------

-- | The type of linear performance heuristics.
newtype LinearHeuristic domain node
  = MkLinearHeuristic
    (node -> domain)
  deriving ()

-- | FIXME: doc
applyLinearHeuristic
  :: LinearHeuristic domain node
  -- ^ FIXME: doc
  -> node
  -- ^ FIXME: doc
  -> domain
  -- ^ FIXME: doc
applyLinearHeuristic (MkLinearHeuristic f) = f

-- | FIXME: doc
runLinearHeuristic
  :: forall node domain m g.
     (MonadIO m, Domain domain)
  => EPEG g node
  -- ^ FIXME: doc
  -> LinearHeuristic domain node
  -- ^ FIXME: doc
  -> m (Maybe (SomePEG node))
  -- ^ FIXME: doc
runLinearHeuristic = undefined

-- | FIXME: doc
instance (Domain domain) => Heuristic (LinearHeuristic domain) where
  data HeuristicConfig (LinearHeuristic domain)
    = MkLinearHeuristicConfig
  defaultHeuristicConfig _ = do
    pure MkLinearHeuristicConfig
  runHeuristic cfg epeg h = do
    runLinearHeuristic epeg h

--------------------------------------------------------------------------------

-- | FIXME: doc
matchPattern
  :: forall node var g.
     (Eq node, Ord var, Hashable var)
  => TTerm node var
  -- ^ FIXME: doc
  -> EPEG g node
  -- ^ FIXME: doc
  -> Maybe (HM.HashMap var (EPEG g node))
  -- ^ FIXME: doc
matchPattern = do
  let go :: forall s.
            MHashMap s var (EPEG g node)
         -> TTerm node var
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

-- | FIXME: doc
applyRule
  :: forall node var g.
     (Eq node, Ord var, Hashable var)
  => (TTerm node var, GTerm node var)
  -- ^ FIXME: doc
  -> EPEG g node
  -- ^ FIXME: doc
  -> Maybe (EPEG g node)
  -- ^ FIXME: doc
applyRule (pat, rep) epeg = runST $ MaybeT.runMaybeT $ do
  -- peg <- epegPEG epeg
  undefined

--------------------------------------------------------------------------------

-- | Given a performance heuristic and an 'EPEG', return the 'PEG' subgraph that
--   maximizes the heuristic.
selectBest
  :: (MonadIO m, Heuristic heuristic)
  => heuristic node
  -- ^ FIXME: doc
  -> EPEG g node
  -- ^ FIXME: doc
  -> m (SomePEG node)
  -- ^ FIXME: doc
selectBest heuristic epeg = do
  cfg <- defaultHeuristicConfig Proxy
  maybeResult <- runHeuristic cfg epeg heuristic
  case maybeResult of
    Just r  -> pure r
    Nothing -> fail "DEBUG: selectBest failed"

-- | Given a 'Set' of 'Equation's and an 'EPEG', this will return a new 'EPEG'
--   that is the result of matching and applying one of the equations to the
--   'EPEG'. If there is no place in the 'EPEG' where any of the equations apply
--   (or if the result of applying the equation is something that is already in
--   the graph), then this function will return 'Nothing'.
saturateStep
  :: Set (Equation node Variable)
  -- ^ FIXME: doc
  -> EPEG g node
  -- ^ FIXME: doc
  -> Maybe (EPEG g node)
  -- ^ FIXME: doc
saturateStep eqs epeg = do
  undefined

-- | The internal version of equality saturation.
saturate
  :: (MonadIO m, Heuristic heuristic)
  => Set (Equation node Variable)
  -- ^ FIXME: doc
  -> heuristic node
  -- ^ FIXME: doc
  -> EPEG g node
  -- ^ FIXME: doc
  -> (EPEG g node -> m Bool)
  -- ^ FIXME: doc
  -> (SomePEG node -> m (Maybe a))
  -- ^ FIXME: doc
  -> m [a]
  -- ^ FIXME: doc
saturate eqs heuristic initial timer callback = do
  let go epeg soFar = do
        case saturateStep eqs epeg of
          Just epeg' -> do let recurse = go epeg'
                           shouldSelectBest <- timer epeg'
                           if shouldSelectBest
                             then selectBest heuristic epeg'
                                  >>= callback
                                  >>= \case (Just x) -> recurse (x : soFar)
                                            Nothing  -> recurse soFar
                             else recurse soFar
          Nothing    -> pure soFar
  go initial []

-- | The public interface of equality saturation.
equalitySaturation
  :: forall node heuristic expr m a.
     ( IsExpression node expr
     , MonadIO m
     , MonadError SomeException m
     , Heuristic heuristic
     )
  => Set (Equation node Variable)
  -- ^ A set of optimization axioms.
  -> heuristic node
  -- ^ The performance heuristic to optimize.
  -> expr
  -- ^ The code whose performance will be optimized.
  -> (forall g. EPEG g node -> m Bool)
  -- ^ A callback that, given the current state of the 'EPEG', will decide
  --   whether we should run 'selectBest' again. In many cases, this will be
  --   some kind of timer.
  -> (expr -> m (Maybe a))
  -- ^ A callback that will be called with the optimized 'Term' every time
  --   'selectBest' has found a new best version of the original program.
  --   The argument is the new best version, and the return value will be
  --   collected in a list during the equality saturation loop. If 'Nothing'
  --   is ever returned by this callback, equality saturation will terminate
  --   early; otherwise it will run for an amount of time that is exponential
  --   in the size of the original program.
  -> m [a]
  -- ^ The list of results produced by the second callback, in _reverse_
  --   chronological order (e.g.: starting with newest and ending with oldest).
equalitySaturation eqs heuristic initial timer cb
  = let exprToEPEG = exprToGTerm .> makePEG' .> pegToEPEG
        pegToExpr (MkSomePEG peg) = case gtermToExpr (pegToTerm peg) of
                                      Left  exception -> throwError exception
                                      Right result    -> pure result
    in saturate eqs heuristic (exprToEPEG initial) timer (pegToExpr >=> cb)

--------------------------------------------------------------------------------
