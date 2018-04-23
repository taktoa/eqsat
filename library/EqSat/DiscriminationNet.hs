--------------------------------------------------------------------------------

module EqSat.DiscriminationNet
  ( module EqSat.DiscriminationNet -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import           EqSat.Term          (TTerm)
import qualified EqSat.Term          as Term

import           EqSat.Equation      (Equation)
import qualified EqSat.Equation      as Equation

--------------------------------------------------------------------------------

data DiscriminationNet node var

-- | FIXME: doc
addRule
  :: Equation node var
  -- ^ FIXME: doc
  -> DiscriminationNet node var
  -- ^ FIXME: doc
  -> DiscriminationNet node var
  -- ^ FIXME: doc
addRule = undefined

-- | FIXME: doc
matchTerm
  :: Term.ClosedTTerm node
  -- ^ FIXME: doc
  -> DiscriminationNet node var
  -- ^ FIXME: doc
  -> HashSet (Equation node var)
  -- ^ FIXME: doc
matchTerm = undefined

-- In returning the reduced set of rewrite rules `R' ⊆ R`, the discrimination
-- net is allowed to commit false positives (keeping in `R'` rules which do not
-- match `f`), but it is not allowed to commit any false negative (not keeping
-- in `R'` rules which _do_ match `f`).

--------------------------------------------------------------------------------
