--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  , OpenTerm
  , ClosedTerm
  , fixTerm
  , varTerm
  , nodeTerm
  , caseTerm
  , mapNode
  , freeVars
  ) where

--------------------------------------------------------------------------------

import           Data.Set       (Set)
import qualified Data.Set       as Set

import           Data.Vector    (Vector)
import qualified Data.Vector    as Vector

import           Data.Void      (Void)

import           Flow           ((|>))

import           EqSat.Variable (Variable)

--------------------------------------------------------------------------------

data TermRepr
  = TermReprG
  | TermReprT
  deriving ()

type ReprG = 'TermReprG
type ReprT = 'TermReprT

-- | A type of term trees. Each node in the tree contains a value and has an
--   arbitrary number of children. This type is polymorphic over the type of
--   variables to exploit a trick that allows us to use the same type for terms
--   with and without metasyntactic variables.
data Term (repr :: TermRepr) node var where
  -- | A @ref@ node allows for observable sharing.
  MkRefTerm  :: !Int
             -> Term 'TermReprG node var
  -- | A @var@ node allows for metasyntactic variables.
  MkVarTerm  :: !var
             -> Term repr node var
  -- | A @node@ node allows for the syntax of your language.
  MkNodeTerm :: !node
             -> !(Vector (Term repr node var))
             -> Term repr node var

-- | FIXME: doc
deriving instance (Eq  node, Eq  var) => Eq  (Term repr node var)

-- | FIXME: doc
deriving instance (Ord node, Ord var) => Ord (Term repr node var)

-- | FIXME: doc
type TTerm node var = Term 'TermReprT node var

-- | FIXME: doc
type GTerm node var = Term 'TermReprG node var

instance Functor (Term repr node) where
  fmap f (MkVarTerm  var)           = MkVarTerm (f var)
  fmap f (MkNodeTerm node children) = Vector.map (fmap f) children
                                      |> MkNodeTerm node

--------------------------------------------------------------------------------

-- | An open term may have (metasyntactic) variables of type 'Variable'.
type OpenTerm repr node = Term repr node Variable

-- | A closed term is one without any variables.
type ClosedTerm repr node = Term repr node Void

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
freeVars (MkRefTerm  ref)        = Set.empty
freeVars (MkVarTerm  var)        = Set.singleton var
freeVars (MkNodeTerm _ children) = Vector.map freeVars children
                                   |> Vector.toList |> Set.unions

--------------------------------------------------------------------------------
