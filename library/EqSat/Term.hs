--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Term where
--   ( Term (MkRefTerm, MkVarTerm, MkNodeTerm)
--     -- FIXME: ↑ don't export these constructors
--   , TermRepr (TermReprG, TermReprT)
--   , ReprG
--   , ReprT
--   , GTerm
--   , TTerm
--   , OpenTerm
--   , ClosedTerm
--   , fixTerm
--   , varTerm
--   , nodeTerm
--   , caseTerm
--   , mapNode
--   , freeVars
--   , coerceTTermToGTerm
--   ) where

--------------------------------------------------------------------------------

import           Data.Set           (Set)
import qualified Data.Set           as Set

import           Data.Vector        (Vector)
import qualified Data.Vector        as Vector

import           Data.Proxy         (Proxy (Proxy))
import           Data.Type.Equality ((:~:) (Refl))
import           Data.Void          (Void)
import           GHC.Generics       (Generic)
import           GHC.TypeLits       (type (+), KnownNat, Nat, natVal, sameNat)


import           Flow               ((|>))

import           EqSat.Variable     (Variable)

import           Refined            (FromTo, NonNegative, Refined)

--------------------------------------------------------------------------------

-- | FIXME: doc
data TermRepr
  = -- | FIXME: doc
    TermReprG
  | -- | FIXME: doc
    TermReprT
  deriving (Generic)

-- | FIXME: doc
type ReprG = 'TermReprG

-- | FIXME: doc
type ReprT = 'TermReprT

--------------------------------------------------------------------------------

-- | A type of term trees. Each node in the tree contains a value and has an
--   arbitrary number of children. This type is polymorphic over the type of
--   variables to exploit a trick that allows us to use the same type for terms
--   with and without metasyntactic variables.
data TermWithDepth (repr :: TermRepr) (depth :: Nat) node var where
  -- | A @ref@ node allows for observable sharing.
  MkRef  :: !(Refined (FromTo 0 depth) Int)
         -> TermWithDepth 'TermReprG depth node var
  -- | A @var@ node allows for metasyntactic variables.
  MkVar  :: !var
         -> TermWithDepth repr depth node var
  -- | A @node@ node allows for the syntax of your language.
  MkNode :: !node
         -> !(Vector (TermWithDepth repr (depth + 1) node var))
         -> TermWithDepth repr depth node var

-- | FIXME: doc
deriving instance ( Eq node, Eq var
                  ) => Eq (TermWithDepth repr depth node var)

-- | FIXME: doc
deriving instance ( Ord node, Ord var
                  ) => Ord (TermWithDepth repr depth node var)

-- | FIXME: doc
instance Functor (TermWithDepth repr depth node) where
  fmap _ (MkRef  ref)     = MkRef ref
  fmap f (MkVar  var)     = MkVar (f var)
  fmap f (MkNode node cs) = Vector.map (fmap f) cs
                            |> MkNode node

-- | FIXME: doc
type TTermWithDepth depth node var = TermWithDepth 'TermReprT depth node var

-- | FIXME: doc
type GTermWithDepth depth node var = TermWithDepth 'TermReprG depth node var

--------------------------------------------------------------------------------

-- | FIXME: doc
data Term (repr :: TermRepr) node var where
  -- | FIXME: doc
  MkTerm :: (KnownNat depth)
         => TermWithDepth repr depth node var
         -> Term repr node var

instance (Eq node, Eq var) => Eq (Term repr node var) where
  (==) = (\(MkTerm a) (MkTerm b) -> go a b)
    where
      go :: forall d1 d2.
            (KnownNat d1, KnownNat d2)
         => TermWithDepth repr d1 node var
         -> TermWithDepth repr d2 node var
         -> Bool
      go a b = case sameNat (Proxy @d1) (Proxy @d2) of
                 Just Refl -> (a == b)
                 Nothing   -> False

instance (Ord node, Ord var) => Ord (Term repr node var) where
  compare = (\(MkTerm a) (MkTerm b) -> go a b)
    where
      go :: forall d1 d2.
            (KnownNat d1, KnownNat d2)
         => TermWithDepth repr d1 node var
         -> TermWithDepth repr d2 node var
         -> Ordering
      go a b = case sameNat (Proxy @d1) (Proxy @d2) of
                 Just Refl -> compare a b
                 Nothing   -> compare (natVal (Proxy @d1)) (natVal (Proxy @d2))

-- | FIXME: doc
instance Functor (Term repr node) where
  fmap f (MkTerm twd) = MkTerm (fmap f twd)

-- | FIXME: doc
type TTerm node var = Term 'TermReprT node var

-- | FIXME: doc
type GTerm node var = Term 'TermReprG node var

--------------------------------------------------------------------------------

-- | An open term may have (metasyntactic) variables of type 'Variable'.
type OpenTerm repr node = Term repr node Variable

-- | A closed term is one without any variables.
type ClosedTerm repr node = Term repr node Void

--------------------------------------------------------------------------------

-- | FIXME: doc
fixTWD
  :: (Eq var)
  => var
  -- ^ FIXME: doc
  -> TermWithDepth repr depth node var
  -- ^ FIXME: doc
  -> GTermWithDepth depth node var
  -- ^ FIXME: doc
fixTWD
  = undefined -- FIXME: replace all matching var nodes with refs at their depth

-- | FIXME: doc
varTWD
  :: var
  -- ^ FIXME: doc
  -> TermWithDepth repr depth node var
  -- ^ FIXME: doc
varTWD = MkVar

-- | FIXME: doc
nodeTWD
  :: node
  -- ^ FIXME: doc
  -> Vector (TermWithDepth repr (depth + 1) node var)
  -- ^ FIXME: doc
  -> TermWithDepth repr depth node var
  -- ^ FIXME: doc
nodeTWD = MkNode

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
caseTerm = undefined
-- caseTerm f _ (MkVar  var)           = f var
-- caseTerm _ f (MkNode node children) = f node children

--------------------------------------------------------------------------------

-- | FIXME: doc
mapNode
  :: (nodeA -> nodeB)
  -- ^ FIXME: doc
  -> Term repr nodeA var
  -- ^ FIXME: doc
  -> Term repr nodeB var
  -- ^ FIXME: doc
mapNode f (MkTerm twd) = MkTerm (mapNodeTWD f twd)

-- | Get the 'Set' of free variables in the given 'Term'.
freeVars
  :: (Ord var)
  => Term repr node var
  -- ^ A term.
  -> Set var
  -- ^ The set of free variables in the given term.
freeVars (MkTerm twd) = freeVarsTWD twd

-- | FIXME: doc
coerceTTermToGTerm
  :: TTerm node var
  -- ^ FIXME: doc
  -> GTerm node var
  -- ^ FIXME: doc
coerceTTermToGTerm (MkTerm twd) = MkTerm (coerceTTWDToGTWD twd)

--------------------------------------------------------------------------------

-- | FIXME: doc
mapNodeTWD
  :: (nodeA -> nodeB)
  -- ^ FIXME: doc
  -> TermWithDepth repr depth nodeA var
  -- ^ FIXME: doc
  -> TermWithDepth repr depth nodeB var
  -- ^ FIXME: doc
mapNodeTWD _ (MkRef  ref)     = MkRef ref
mapNodeTWD _ (MkVar  var)     = MkVar var
mapNodeTWD f (MkNode node cs) = Vector.map (mapNodeTWD f) cs
                                |> MkNode (f node)

-- | Get the 'Set' of free variables in the given 'TermWithDepth'.
freeVarsTWD
  :: (Ord var)
  => TermWithDepth repr depth node var
  -- ^ A term.
  -> Set var
  -- ^ The set of free variables in the given 'TermWithDepth'.
freeVarsTWD (MkRef  _)          = Set.empty
freeVarsTWD (MkVar  var)        = Set.singleton var
freeVarsTWD (MkNode _ children) = Vector.map freeVarsTWD children
                                  |> Vector.toList |> Set.unions

-- | FIXME: doc
coerceTTWDToGTWD
  :: TTermWithDepth depth node var
  -- ^ FIXME: doc
  -> GTermWithDepth depth node var
  -- ^ FIXME: doc
coerceTTWDToGTWD (MkVar  var)     = MkVar var
coerceTTWDToGTWD (MkNode node cs) = Vector.map coerceTTWDToGTWD cs
                                    |> MkNode node

--------------------------------------------------------------------------------
