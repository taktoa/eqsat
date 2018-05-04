--------------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UnboxedTuples          #-}

--------------------------------------------------------------------------------

module EqSat.TermIndex.DiscriminationNet
  ( module EqSat.TermIndex.DiscriminationNet -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Monad               (join, void)
import           Control.Monad.Fail          (MonadFail)
import           Control.Monad.Primitive
import           Control.Monad.ST            (ST)
import           Control.Monad.Trans.Class   (MonadTrans (lift))

import           Data.Maybe
import           Data.Monoid                 ((<>))

import qualified Data.Foldable               as Foldable
import           Data.Functor.Identity       (Identity)

import           Data.Hashable               (Hashable)

import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HashSet

import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap

import           Data.Vector                 (Vector)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GMV
import           Data.Vector.Mutable         (MVector)
import qualified Data.Vector.Unboxed         as UVector
import qualified Data.Vector.Unboxed.Mutable as UMVector

import           Data.Primitive.MutVar       (MutVar)
import qualified Data.Primitive.MutVar       as MutVar

import           Data.Primitive.ByteArray    (ByteArray, MutableByteArray)
import qualified Data.Primitive.ByteArray    as ByteArray
import qualified Data.Primitive.ByteArray    as MutableByteArray

import           Flow

import           GHC.Prim                    (Int#)
import qualified GHC.Prim
import qualified GHC.Types

import           EqSat.Term
                 (GTerm, TTerm, Term (MkNodeTerm, MkVarTerm))
import qualified EqSat.Term                  as Term

import           EqSat.Equation              (Equation)
import qualified EqSat.Equation              as Equation

import           EqSat.Internal.MHashSet     (MHashSet)
import qualified EqSat.Internal.MHashSet     as MHashSet

import           EqSat.Internal.MStack       (MStack)
import qualified EqSat.Internal.MStack       as MStack

import           EqSat.Internal.Refined      (NonNegative, Refined)
import qualified EqSat.Internal.Refined      as Refined

--------------------------------------------------------------------------------

data DiscriminationNet -- FIXME: remove this placeholder

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
class (Monad m, Eq node) => TreeLike m node where
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
    -> m (Vector node)
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
  default numChildren :: (MonadFail m) => node -> m (Refined NonNegative Int)
  numChildren n = do
    len <- length <$> childrenOf n
    Refined.refineFail len

  -- | Given a node @n ∷ node@ and a function @f ∷ node → m a@, run @f@ on each
  --   child of @n@ in order, collecting the results into a @[a]@.
  --
  --   Laws:
  --
  --   1. For any @n ∷ node@ and @f ∷ node → m a@,
  --      @'forChildren' n f ≡ 'childrenOf' n '>>=' 'Vector.mapM' f@.
  forChildren
    :: node
    -- ^ A node @n@ whose children will be traversed in order.
    -> (node -> m a)
    -- ^ A function @f@ that will be run on each child.
    -> m (Vector a)
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
  --      @'forChildren_' n f ≡ 'childrenOf' n '>>=' 'Vector.mapM_' f@.
  --   2. For any @n ∷ node@ and @f ∷ node → m a@,
  --      @'forChildren_' n f ≡ 'forChildren' n f '>>' 'pure' ()@.
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
  :: forall m node vec a.
     (PrimMonad m, GV.Vector vec a, TreeLike m node)
  => node
  -> (node -> m a)
  -> m (vec a)
preorderTraversal n visit = do
  output <- MStack.new
  stack  <- MStack.new :: m (MStack.BMStack (PrimState m) node)
  MStack.push stack n
  let iterativeTraversal :: m ()
      iterativeTraversal = do
        join $ MStack.pop stack (pure ()) $ \node -> do
          visit node >>= MStack.push output
          forChildren node $ \child -> do
            MStack.push stack child
          iterativeTraversal
  iterativeTraversal
  MStack.freeze output

preorderTraversal_
  :: forall m node a.
     (PrimMonad m, TreeLike m node)
  => node
  -> (node -> m a)
  -> m ()
preorderTraversal_ n visit = do
  stack <- MStack.new :: m (MStack.BMStack (PrimState m) node)
  MStack.push stack n
  let iterativeTraversal :: m ()
      iterativeTraversal = do
        join $ MStack.pop stack (pure ()) $ \node -> do
          visit node
          forChildren node $ \child -> do
            MStack.push stack child
          iterativeTraversal
  iterativeTraversal

--------------------------------------------------------------------------------

-- | FIXME: doc
instance (MonadFail m, Eq node, Eq var) => TreeLike m (TTerm node var) where
  childrenOf
    :: TTerm node var
    -> m (Vector (TTerm node var))
  childrenOf (Term.MkVarTerm  _)          = pure mempty
  childrenOf (Term.MkNodeTerm _ children) = pure children

  numChildren
    :: TTerm node var
    -> m (Refined NonNegative Int)
  numChildren n = do
    len <- length <$> childrenOf n
    Refined.refineFail len

  forChildren
    :: TTerm node var
    -> (TTerm node var -> m a)
    -> m (Vector a)
  forChildren n f = do
    cs <- childrenOf n
    mapM f cs

  forChildren_
    :: TTerm node var
    -> (TTerm node var -> m a)
    -> m ()
  forChildren_ n f = do
    cs <- childrenOf n
    mapM_ f cs

-- FIXME: add instance `(Eq node, Eq var) => TreeLike Identity (GTerm node var)`

preorderTerm
  :: TTerm node var
  -> Vector (Either node var)
preorderTerm = undefined

--------------------------------------------------------------------------------

-- | FIXME: doc
class Trie (t :: * -> * -> *) where
  -- | FIXME: doc
  data Mut t :: * -> * -> * -> *

  -- | FIXME: doc
  mutTrieInsertWith
    :: (PrimMonad m, GV.Vector vec k)
    => Mut t (PrimState m) k v
    -- ^ A mutable trie.
    -> vec k
    -- ^ A key-vector.
    -> v
    -- ^ The value to associate with this key-vector.
    -> (v -> v -> m v)
    -- ^ A callback that will be run with the old and new values respectively
    --   to compute the result value, if the same key-vector has already been
    --   inserted into the trie.
    -> m ()
    -- ^ A 'PrimMonad' action that associates the given key-vector with the
    --   given value in the given mutable trie, using the given callback if
    --   such an association was already present in the

  -- | FIXME: doc
  mutTrieFindChild
    :: (PrimMonad m)
    => Mut t (PrimState m) k v
    -- ^ A mutable trie node.
    -> k
    -- ^ Key to find in the children of the current node.
    -> (v -> m a)
    -- ^ Callback to run if this is a leaf node.
    -> (Maybe (Mut t (PrimState m) k v) -> m a)
    -- ^ Callback to run if this is a branch node.
    -> m a
    -- ^ The result of the relevant callback.

  -- | FIXME: doc
  mutTrieForChildren
    :: (PrimMonad m)
    => Mut t (PrimState m) k v
    -- ^ FIXME: doc
    -> (k -> m a)
    -- ^ FIXME: doc
    -> m [a]
    -- ^ FIXME: doc

  -- | FIXME: doc
  mutTrieForChildren_
    :: (PrimMonad m)
    => Mut t (PrimState m) k v
    -- ^ FIXME: doc
    -> (k -> m ())
    -- ^ FIXME: doc
    -> m ()
    -- ^ FIXME: doc
  mutTrieForChildren_ trie callback = do
    mutTrieForChildren trie callback
    pure ()

--------------------------------------------------------------------------------

data Children
  = MkChildren
    {-# UNPACK #-} !(# Int# , Int# , Int# , Int# #)
  deriving ()

data MDiscriminationNet s node var
  = MkMDiscriminationNet
    { _MDiscriminationNet_structure :: {-# UNPACK #-} !(MutableByteArray s)
    , _MDiscriminationNet_nodes     ::                !(MVector s node)
    , _MDiscriminationNet_variables ::                !(MVector s var)
    }
    -- !(MVector s (MDiscriminationNet s node var))
  deriving ()

indexOfChildFast
  :: (PrimMonad m)
  => MDiscriminationNet (PrimState m) node var
  -> Int#
  -> Int#
  -> m Int
indexOfChildFast = undefined

-- | FIXME: doc
new
  :: (PrimMonad m)
  => m (MDiscriminationNet (PrimState m) node var)
  -- ^ FIXME: doc
new = undefined -- MkDNDisj <$> GMV.new 0

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
