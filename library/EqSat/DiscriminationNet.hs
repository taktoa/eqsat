--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

--------------------------------------------------------------------------------

module EqSat.DiscriminationNet
  ( module EqSat.DiscriminationNet -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Monad           (void)
import           Control.Monad.Primitive
import           Control.Monad.ST        (ST)

import           Data.Maybe

import qualified Data.Foldable           as Foldable
import           Data.Functor.Identity   (Identity)

import           Data.Hashable           (Hashable)

import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as HashSet

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap

import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector

import           Data.Vector.Mutable     (MVector)
import qualified Data.Vector.Mutable     as MVector

import           Data.STRef              (STRef)
import qualified Data.STRef              as STRef

import           Flow

import           EqSat.Term              (TTerm, Term (MkNodeTerm, MkVarTerm))
import qualified EqSat.Term              as Term

import           EqSat.Equation          (Equation)
import qualified EqSat.Equation          as Equation

import           EqSat.Internal.MHashSet (MHashSet)
import qualified EqSat.Internal.MHashSet as MHashSet

import           Refined                 (NonNegative, Refined)
import qualified Refined

--------------------------------------------------------------------------------

-- | A typeclass abstracting the functions that tree-like types have.
--
--   Specifically, I'm talking about rooted directed graphs with labeled edges
--   such that the type of labels is totally ordered and no two edges out of the
--   same vertex have the same label.
--
--   If the tree is pure (i.e.: an ADT), then @m@ will probably be 'Identity'.
--
--   If the tree is impure, then @m@ will probably be @('PrimMonad' m) ⇒ m@
--   or @'ST' s@ or 'IO'.
class (Monad m, Eq node) => TreeLike m node | node -> m where
  {-# MINIMAL childrenOf | forChildren #-}

  -- | Returns a 'Vector' containing all of the children of the given node
  --   in the order given by sorting the edge labels in increasing order.
  --
  --   Laws:
  --
  --   1. For any @n ∷ node@, @'childrenOf' n ≡ 'forChildren' n 'pure'@.
  childrenOf
    :: node
    -- ^ A node.
    -> m [node]
    -- ^ An @m@ action returning a 'Vector' of its children, in order.
  childrenOf n = forChildren n pure

  -- | Given a node, return an action that returns an integer ('Refined' so we
  --   know it is not negative) representing the number of children that that
  --   node has.
  --
  --   Laws:
  --
  --   1. For any @n ∷ node@,
  --      @'unrefine' '<$>' 'numChildren' n ≡ 'length' '<$>' 'childrenOf' n@.
  numChildren
    :: node
    -- ^ A node.
    -> m (Refined NonNegative Int)
    -- ^ An action returning the number of children that node has.
  numChildren n = do
    len <- Vector.length <$> childrenOf n
    either fail pure (Refined.refine len)

  -- | Given a node @n ∷ node@ and a function @f ∷ node → m a@, run @f@ on each
  --   child of @n@ in order, collecting the results into a @[a]@.
  --
  --   Laws:
  --
  --   1. For any @n ∷ node@ and @f ∷ node → m a@,
  --      @'forChildren' n f ≡ 'childrenOf' n >>= 'Vector.mapM' f@.
  forChildren
    :: node
    -- ^ A node @n@ whose children will be traversed in order.
    -> (node -> m a)
    -- ^ A function @f@ that will be run on each child.
    -> m [a]
    -- ^ An action in the @m@ monad that has the effects associated with the
    --   return values of all of the @f@ executions, sequenced according to
    --   the order of the children, collecting the results in a list.
  forChildren n f = do
    cs <- childrenOf n
    mapM f cs

  -- | Given a node @n ∷ node@ and a function @f ∷ node → m a@, run @f@ on each
  --   child of @n@ in order.
  --
  --   Laws:
  --
  --   1. For any @n ∷ node@ and @f ∷ node → m a@,
  --      @'forChildren_' n f ≡ 'childrenOf' n >>= 'Vector.mapM_' f@.
  --   2. For any @n ∷ node@ and @f ∷ node → m a@,
  --      @'forChildren_' n f ≡ 'forChildren' n f >> 'pure' ()@.
  forChildren_
    :: node
    -- ^ A node @n@ whose children will be traversed in order.
    -> (node -> m a)
    -- ^ A function @f@ that will be run on each child.
    -> m ()
    -- ^ An action in the @m@ monad that has the effects associated with the
    --   return values of all of the @f@ executions, sequenced according to
    --   the order of the children.
  forChildren_ n f = do
    cs <- childrenOf n
    mapM_ f cs

preorderTraversal
  :: (TreeLike m node)
  => node
  -> (node -> m a)
  -> m (Vector a)
preorderTraversal n f = do
  vecs <- forChildren n $ \child -> do
    first <- f child
    rest  <- Vector.toList <$> preorderTraversal child f
    pure (Vector.fromList (first : rest))
  pure (Vector.concat (Vector.toList vecs))

preorderTraversal_
  :: (TreeLike m node)
  => node
  -> (node -> m a)
  -> m ()
preorderTraversal_ n f = do
  forChildren_ n $ \child -> do
    preorderTraversal_ child f

--------------------------------------------------------------------------------

-- instance TreeLike Identity (TTerm node var) where

--------------------------------------------------------------------------------



-- class PatternIndex index where
--   addRule
--     :: (PrimMonad m, Ord node, Hashable node, Ord var, Hashable var)
--     => Equation node var
--     -- ^ FIXME: doc
--     -> index (PrimState m) node var
--     -- ^ FIXME: doc
--     -> m ()
--     -- ^ FIXME: doc
--   queryRule
--     :: TreeLike (PrimState m) t
--     -> (t -> m (Either node))

-- pattern is (+ x (+ y z))
-- graph is
--     +_A --0--> 3
--     +_A --1--> +_B
--     +_B --0--> 5
--     +_B --1--> 7
-- (we need separate +_A and +_B because graphs have a set of vertices)
--
-- matching algorithm looks like this:
-- Input: a pattern (e.g.: `(+ x (+ y z))`) and a node in the graph (e.g.: `+_A`)
-- Step 1: If the top-level node of the pattern is a metavariable `m` and the node we are matching is `v`, return `Map.singleton m v`
-- Step 2: Otherwise, the top-level node of the pattern is a constructor. Compare the constructor name (e.g.: `+`) to the label of the node in the graph (e.g.: `+`). If they are not equal, fail.
-- Step 3: Suppose that the list of children of the top-level node of the pattern is called `pats :: [Pat]` and the list of children of the node in the graph paired with their edge weights is called `edges :: [(Int, Node)]`.
-- Then return `Map.unionWith (\x y -> error "nonlinear not allowed") <$> mapM (uncurry match) (zip pats (map fst (sortBy (comparing fst) edges)))`.

data MDiscriminationNet s node var
  = MkDNNode
    !(STRef s node)
    !(MVector s (MDiscriminationNet s node var))
  | MkDNDisj
    !(MVector s (MDiscriminationNet s node var))
  | MkDNEmit
    !(MHashSet s (Equation node var))
  deriving ()

-- | FIXME: doc
new
  :: (PrimMonad m)
  => m (MDiscriminationNet (PrimState m) node var)
  -- ^ FIXME: doc
new = MkDNDisj <$> MVector.new 0

-- Level-order traversal to get ([Either node var], rhs)
-- Smoosh all those lists together to get a tree.

-- levelOrder
--   ::
--   -> Vector (Either node var)

newtype Position
  = MkPosition (Vector Int)

newtype PString a
  = MkPString (Vector (Position, a))

makePString :: TTerm node var -> PString (Either node var)
makePString = undefined



-- | FIXME: doc
addRule
  :: forall node var m.
     (PrimMonad m, Hashable node, Hashable var)
  => Equation node var
  -- ^ FIXME: doc
  -> MDiscriminationNet (PrimState m) node var
  -- ^ FIXME: doc
  -> m ()
  -- ^ FIXME: doc
addRule eq dn = do
  undefined
    -- (\dn -> fromMaybe dn (go (Equation.getLHS eq) dn))
    -- go :: TTerm node var
    --    -> DiscriminationNet node var
    --    -> Maybe (DiscriminationNet node var)
    -- go (Term.MkVarTerm var)
    --   = \case (MkDNNode node children) -> [ MkDNEmit (HashSet.singleton eq)
    --                                       , MkDNNode node children
    --                                       ] |> mkDisj |> pure
    --           (MkDNDisj children)      -> children mkDisj
    --           (MkDNEmit equations)     -> undefined
    -- go (Term.MkNodeTerm n cs)
    --   = \case (MkDNNode node children) -> undefined
    --           (MkDNDisj children)      -> undefined
    --           (MkDNEmit equations)     -> undefined
    --
    -- mkNode = \node -> Vector.fromList .> MkDNNode
    -- mkDisj = Vector.fromList .> MkDNDisj
    -- mkEmit = HashSet.fromList .> MkDNEmit
    --
    -- flatten :: DiscriminationNet node var
    --         -> DiscriminationNet node var
    -- flatten (MkDNDisj children) = let c' = Vector.concatMap flattenHelper children

-- | FIXME: doc
matchTerm
  :: (PrimMonad m, Eq node, Eq var, Hashable node, Hashable var)
  => MDiscriminationNet (PrimState m) node var
  -- ^ FIXME: doc
  -> Term.ClosedTTerm node
  -- ^ FIXME: doc
  -> m (HashSet (Equation node var))
  -- ^ FIXME: doc
matchTerm = undefined

-- If A is a node with an outgoing edge to B that creates a cycle, then
-- a pattern only matches A if its node value is the same and the child with
-- the same index as that outgoing edge is a metavariable or if it matches B.

-- In returning the reduced set of rewrite rules `R' ⊆ R`, the discrimination
-- net is allowed to commit false positives (keeping in `R'` rules which do not
-- match `f`), but it is not allowed to commit any false negative (not keeping
-- in `R'` rules which _do_ match `f`).

--------------------------------------------------------------------------------
