--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Term
  ( Term (MkRefTerm, MkVarTerm, MkNodeTerm)
    -- FIXME: ↑ don't export these constructors
  , TermRepr (TermReprG, TermReprT)
  , ReprG
  , ReprT
  , GTerm
  , TTerm
  , OpenTerm, OpenTTerm, OpenGTerm
  , ClosedTerm, ClosedTTerm, ClosedGTerm
  , fixTerm
  , varTerm
  , nodeTerm
  , caseTerm
  , mapNode
  , freeVars
  , coerceTTermToGTerm
  , genTTerm
  , genGTerm
  ) where

--------------------------------------------------------------------------------

import           Control.Monad          (forM)
import           Control.Monad.Fail     (MonadFail (fail))

import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector

import           Data.Void              (Void)

import           GHC.Generics           (Generic)

import           Data.Hashable          (Hashable (hashWithSalt))

import           Flow                   ((.>), (|>))

import qualified Hedgehog               as HH
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import           EqSat.Variable         (Variable)

import           EqSat.Internal.Refined
                 (NonNegative, Refined, refine, unrefine, unsafeRefine)

--------------------------------------------------------------------------------

-- | FIXME: doc
data TermRepr
  = -- | FIXME: doc
    TermReprG
  | -- | FIXME: doc
    TermReprT
  deriving (Generic)

-- | FIXME: doc
instance Hashable TermRepr

-- | FIXME: doc
type ReprG = 'TermReprG

-- | FIXME: doc
type ReprT = 'TermReprT

--------------------------------------------------------------------------------

-- | A type of term trees. Each node in the tree contains a value and has an
--   arbitrary number of children. This type is polymorphic over the type of
--   variables to exploit a trick that allows us to use the same type for terms
--   with and without metasyntactic variables.
data Term (repr :: TermRepr) node var where
  -- | A @ref@ node allows for observable sharing.
  MkRefTerm  :: !(Refined NonNegative Int)
             -> Term ReprG node var
  -- | A @var@ node allows for metasyntactic variables.
  MkVarTerm  :: !var
             -> Term repr node var
  -- | A @node@ node allows for the syntax of your language.
  MkNodeTerm :: !node
             -> !(Vector (Term repr node var))
             -> Term repr node var

-- | FIXME: doc
deriving instance (Eq node, Eq var) => Eq (Term repr node var)

-- | FIXME: doc
deriving instance (Ord node, Ord var) => Ord (Term repr node var)

-- | FIXME: doc
deriving instance (Show node, Show var) => Show (Term repr node var)

-- | FIXME: doc
type TTerm node var = Term ReprT node var

-- | FIXME: doc
type GTerm node var = Term ReprG node var

-- | FIXME: doc
instance Functor (Term repr node) where
  fmap _ (MkRefTerm  ref)     = MkRefTerm ref
  fmap f (MkVarTerm  var)     = MkVarTerm (f var)
  fmap f (MkNodeTerm node cs) = Vector.map (fmap f) cs
                                |> MkNodeTerm node

-- | FIXME: doc
instance (Hashable node, Hashable var) => Hashable (TTerm node var) where
  hashWithSalt salt
    = (\case (MkVarTerm var) ->
               salt `hashWithSalt` var
             (MkNodeTerm node cs) ->
               Vector.foldl' hashWithSalt (salt `hashWithSalt` node) cs)

-- | FIXME: doc
instance (Hashable node, Hashable var) => Hashable (GTerm node var) where
  hashWithSalt salt
    = (\case (MkRefTerm ref) ->
               salt `hashWithSalt` unrefine ref
             (MkVarTerm var) ->
               salt `hashWithSalt` var
             (MkNodeTerm node cs) ->
               Vector.foldl' hashWithSalt (salt `hashWithSalt` node) cs)

--------------------------------------------------------------------------------

-- | An open term may have (metasyntactic) variables of type 'Variable'.
type OpenTerm repr node = Term repr node Variable

-- | A closed term is one without any variables.
type ClosedTerm repr node = Term repr node Void

-- | An open term may have (metasyntactic) variables of type 'Variable'.
type OpenTTerm node = OpenTerm ReprT node

-- | A closed term is one without any variables.
type ClosedTTerm node = ClosedTerm ReprT node

-- | An open term may have (metasyntactic) variables of type 'Variable'.
type OpenGTerm node = OpenTerm ReprG node

-- | A closed term is one without any variables.
type ClosedGTerm node = ClosedTerm ReprG node

--------------------------------------------------------------------------------

-- | FIXME: doc
fixTerm
  :: (Eq var)
  => var
  -- ^ FIXME: doc
  -> Term repr node var
  -- ^ FIXME: doc
  -> GTerm node var
  -- ^ FIXME: doc
fixTerm
  = undefined -- FIXME: replace all matching var nodes with refs at their depth

-- | FIXME: doc
varTerm
  :: var
  -- ^ FIXME: doc
  -> Term repr node var
  -- ^ FIXME: doc
varTerm = MkVarTerm

-- | FIXME: doc
nodeTerm
  :: node
  -- ^ FIXME: doc
  -> Vector (Term repr node var)
  -- ^ FIXME: doc
  -> Term repr node var
  -- ^ FIXME: doc
nodeTerm = MkNodeTerm

--------------------------------------------------------------------------------

-- | FIXME: doc
caseTerm
  :: (var -> result)
  -- ^ FIXME: doc
  -> (node -> Vector (Term repr node var) -> result)
  -- ^ FIXME: doc
  -> Term repr node var
  -- ^ FIXME: doc
  -> result
  -- ^ FIXME: doc
caseTerm f _ (MkVarTerm  var)           = f var
caseTerm _ f (MkNodeTerm node children) = f node children

--------------------------------------------------------------------------------

-- | FIXME: doc
mapNode
  :: (nodeA -> nodeB)
  -- ^ FIXME: doc
  -> Term repr nodeA var
  -- ^ FIXME: doc
  -> Term repr nodeB var
  -- ^ FIXME: doc
mapNode _ (MkRefTerm  ref)           = MkRefTerm ref
mapNode _ (MkVarTerm  var)           = MkVarTerm var
mapNode f (MkNodeTerm node children) = Vector.map (mapNode f) children
                                       |> MkNodeTerm (f node)

-- | Get the 'Set' of free variables in the given 'Term'.
freeVars
  :: (Ord var)
  => Term repr node var
  -- ^ A term.
  -> Set var
  -- ^ The set of free variables in the given term.
freeVars (MkRefTerm  _)          = Set.empty
freeVars (MkVarTerm  var)        = Set.singleton var
freeVars (MkNodeTerm _ children) = Vector.map freeVars children
                                   |> Vector.toList |> Set.unions

-- | FIXME: doc
coerceTTermToGTerm :: TTerm node var -> GTerm node var
coerceTTermToGTerm (MkVarTerm  var)     = MkVarTerm var
coerceTTermToGTerm (MkNodeTerm node cs) = Vector.map coerceTTermToGTerm cs
                                          |> MkNodeTerm node

--------------------------------------------------------------------------------

-- | FIXME: doc
genTTerm
  :: forall m node var.
     (HH.MonadGen m, MonadFail m)
  => Int
  -- ^ FIXME: doc
  -> Set var
  -- ^ FIXME: doc
  -> (Int -> m (node, Int))
  -- ^ Given an 'Int' representing the maximum arity of the function symbol,
  --   return a generator for a function symbol and its arity.
  -> m (TTerm node var)
  -- ^ FIXME: doc
genTTerm depth vars genNode = do
  let varList = Set.toList vars

  let genConstant :: m (TTerm node var)
      genConstant = do
        (label, 0) <- genNode 0
        pure (MkNodeTerm label mempty)

  let genVar :: m (TTerm node var)
      genVar = do
        Gen.choice (map (MkVarTerm .> pure) varList)

  let go :: Int -> m (TTerm node var)
      go n | (n <= 1) = do
               Gen.choice [genConstant, genVar]
           | otherwise = do
               Gen.choice
                 [ go 0
                 , do (label, k) <- genNode maxBound
                      children <- Vector.replicateM k (go (n `div` k))
                      pure (MkNodeTerm label children)
                 ]

  go depth

-- | FIXME: doc
genGTerm
  :: forall m node var.
     (HH.MonadGen m)
  => Int
  -- ^ FIXME: doc
  -> Set var
  -- ^ FIXME: doc
  -> (Int -> m (node, Int, Set Int))
  -- ^ Given an 'Int' representing the maximum arity of the function symbol,
  --   return a generator for a function symbol, its arity, and the set of
  --   children indices (0-indexed) that can refer back to this node.
  -> m (GTerm node var)
  -- ^ FIXME: doc
genGTerm depth vars genNode = do
  let varList = Set.toList vars

  let genConstant :: m (GTerm node var)
      genConstant = do
        (label, _, _) <- genNode 0
        pure (MkNodeTerm label mempty)

  let genVar :: m (GTerm node var)
      genVar = do
        Gen.choice (map (MkVarTerm .> pure) varList)

  let genRef :: Set Int -> m (GTerm node var)
      genRef rs = do
        r <- unsafeRefine <$> Gen.element (Set.toList rs)
        pure (MkRefTerm r)

  let go :: Int -> Set Int -> m (GTerm node var)
      go n rs
        | (n <= 1) = do
            Gen.choice [genConstant, genVar, genRef rs]
        | otherwise = do
            Gen.choice
              [ go 0 rs
              , do (label, k, referables) <- genNode maxBound
                   children <- forM [0 .. k - 1] $ \i -> do
                     go (n `div` k)
                        (Set.map succ (if i `Set.member` referables
                                       then Set.insert 0 rs
                                       else rs))
                   pure (MkNodeTerm label (Vector.fromList children))
              ]

  go depth Set.empty

--------------------------------------------------------------------------------
