--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Equation
  ( Equation
  , make
  , from
  , boundVariables
  , getLHS
  , getRHS
  ) where

--------------------------------------------------------------------------------

import           Control.Monad (guard)

import           Data.Set      (Set)
import qualified Data.Set      as Set

import           EqSat.Term    (GTerm, TTerm, Term, freeVars)
import qualified EqSat.Term    as Term

import           Data.Hashable (Hashable (hashWithSalt))

import           GHC.Generics  (Generic)

--------------------------------------------------------------------------------

-- | FIXME: doc
data Equation node var
  = UnsafeMkEquation
    !(TTerm node var)
    !(GTerm node var)
    !(Set var)
  deriving (Generic)

instance (Eq node, Eq var) => Eq (Equation node var) where
  a == b = (from a) == (from b)

instance (Ord node, Ord var) => Ord (Equation node var) where
  compare a b = compare (from a) (from b)

-- | FIXME: doc
instance (Hashable node, Hashable var) => Hashable (Equation node var) where
  hashWithSalt salt (UnsafeMkEquation lhs rhs _)
    = salt `hashWithSalt` lhs `hashWithSalt` rhs

--------------------------------------------------------------------------------

-- | Smart constructor for 'Equation's.
--
--   The set of free variables in the given right-hand side must be a subset
--   of the set of free variables in the given left-hand side, or else 'Nothing'
--   will be returned.
--
--   Laws:
--   * For any @(lhs, rhs) ∈ ('Term' node var, 'Term' node var)@,
--     @'Set.isSubsetOf' ('freeVars' rhs) ('freeVars' lhs) ≡ 'True'@
--     implies that
--     @'fromEquation' '<$>' 'makeEquation' (lhs, rhs) ≡ 'Just' (lhs, rhs)@.
--   * For any @(lhs, rhs) ∈ ('Term' node var, 'Term' node var)@,
--     @'Set.isSubsetOf' ('freeVars' rhs) ('freeVars' lhs) ≡ 'False'@
--     implies that @'makeEquation' (lhs, rhs) ≡ 'Nothing'@.
make
  :: (Ord var)
  => (TTerm node var, GTerm node var)
  -- ^ A pair @(lhs, rhs)@ containing the left- and right-hand sides
  --   respectively of the would-be equation.
  -> Maybe (Equation node var)
  -- ^ The equation, if the given @(lhs, rhs)@ pair was valid.
make (lhs, rhs) = do
  let (freeLHS, freeRHS) = (freeVars lhs, freeVars rhs)
  guard (freeRHS `Set.isSubsetOf` freeLHS)
  pure (UnsafeMkEquation lhs rhs freeLHS)

-- | Get a pair containing the left- and right-hand sides of the given equation.
from
  :: Equation node var
  -- ^ An equation.
  -> (TTerm node var, GTerm node var)
  -- ^ A pair @(lhs, rhs)@ containing the left- and right-hand sides
  --   respectively of the given equation.
from (UnsafeMkEquation lhs rhs _) = (lhs, rhs)

-- | Get the set of variables bound in this equation by the left-hand side.
boundVariables
  :: Equation node var
  -- ^ An equation.
  -> Set var
  -- ^ The set of variables bound in this equation by the left-hand side.
boundVariables (UnsafeMkEquation _ _ bounds) = bounds

-- | Helper function for getting the left-hand side of an equation.
getLHS
  :: Equation node var
  -- ^ An equation.
  -> TTerm node var
  -- ^ The left-hand side of the given equation.
getLHS = fst . from

-- | Helper function for getting the right-hand side of an equation.
getRHS
  :: Equation node var
  -- ^ An equation.
  -> GTerm node var
  -- ^ The right-hand side of the given equation.
getRHS = snd . from

--------------------------------------------------------------------------------
