--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Term
  ( Term (MkVarTerm, MkNodeTerm) -- FIXME: don't export these constructors
  , OpenTerm
  , ClosedTerm
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

-- | A type of term trees. Each node in the tree contains a value and has an
--   arbitrary number of children. This type is polymorphic over the type of
--   variables to exploit a trick that allows us to use the same type for terms
--   with and without metasyntactic variables.
data Term node var
  = MkVarTerm var
  | MkNodeTerm node (Vector (Term node var))
  deriving (Eq, Ord)

instance Functor (Term node) where
  fmap f (MkVarTerm  var)           = MkVarTerm (f var)
  fmap f (MkNodeTerm node children) = Vector.map (fmap f) children
                                      |> MkNodeTerm node

--------------------------------------------------------------------------------

-- | An open term may have (metasyntactic) variables of type 'Variable'.
type OpenTerm node = Term node Variable

-- | A closed term is one without any variables.
type ClosedTerm node = Term node Void

--------------------------------------------------------------------------------

-- | FIXME: doc
varTerm
  :: var
  -- ^ FIXME: doc
  -> Term node var
  -- ^ FIXME: doc
varTerm = MkVarTerm

-- | FIXME: doc
nodeTerm
  :: node
  -- ^ FIXME: doc
  -> Vector (Term node var)
  -- ^ FIXME: doc
  -> Term node var
  -- ^ FIXME: doc
nodeTerm = MkNodeTerm

--------------------------------------------------------------------------------

-- | FIXME: doc
caseTerm
  :: (var -> result)
  -- ^ FIXME: doc
  -> (node -> Vector (Term node var) -> result)
  -- ^ FIXME: doc
  -> Term node var
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
  -> Term nodeA var
  -- ^ FIXME: doc
  -> Term nodeB var
  -- ^ FIXME: doc
mapNode f (MkVarTerm  var)           = MkVarTerm var
mapNode f (MkNodeTerm node children) = Vector.map (mapNode f) children
                                       |> MkNodeTerm (f node)

-- | Get the 'Set' of free variables in the given 'Term'.
freeVars
  :: (Ord var)
  => Term node var
  -- ^ A term.
  -> Set var
  -- ^ The set of free variables in the given term.
freeVars (MkVarTerm  var)        = Set.singleton var
freeVars (MkNodeTerm _ children) = Vector.map freeVars children
                                   |> Vector.toList |> Set.unions

--------------------------------------------------------------------------------
