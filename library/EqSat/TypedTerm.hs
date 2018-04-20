--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.TypedTerm
  ( Substitution
  , TypedTerm
  , makeTypedTerm
  , underlyingTerm
  , wholeTermTypingFunction
  , metavariableTypingFunction
  ) where

--------------------------------------------------------------------------------

import           Control.Monad (guard)
import           Data.Maybe    (isJust)

import           Flow          ((.>))

import qualified Data.Set      as Set

import           EqSat.Term    (Term)
import qualified EqSat.Term    as Term

--------------------------------------------------------------------------------

-- | A substitution is basically a partial function.
--
--   We use a function type rather than a 'HashMap' or 'Map' because we want to
--   allow users to use whatever dictionary type makes sense for their
--   application, as long as it supports lookup.
type Substitution a b
  = a -> Maybe b

--------------------------------------------------------------------------------

-- | A 'TypedTerm' consists of three things:
--
--   1. The underlying 'Term'.
--   2. A function, called the /whole-term typing function/, that, given a
--      substitution of metavariables for types, returns the least general type
--      assignable to the result of substituting terms with those types for the
--      metavariables in the underlying term.
--      If the substitution is invalid (e.g.: it returns 'Nothing' for one of
--      the free variables of the underlying 'Term', or it contains a
--      replacement @(x : S) ↦ (y : T)@ such that @S ⊂ T@), then this function
--      must return 'Nothing'.
--   3. A substitution map, called the /metavariable typing function/, from
--      metavariables to their most general types.
--
--   Laws:
--
--   1. The preimage of the metavariable typing function must be the same as the
--      set of free variables of the underlying term.
--   2. Denoting the metavariable typing function @m@ and the whole-term typing
--      function @w@ and the subtyping relation @(⊆) ∈ ty → ty → 'Bool'@,
--      if @θ@ is a substitution such that for every free variable @v@ of the
--      underlying term, @((⊆) '<$>' θ v '<*>' m v) ≡ 'Just' 'True'@, then
--      there exists a type @t@ such that @w θ ≡ 'Just' t@.
data TypedTerm node var ty
  = UnsafeMkTypedTerm
    { _typedTermUnderlyingTerm :: Term node var
    , _typedTermOverallType    :: Substitution var ty -> Maybe ty
    , _typedTermVarType        :: Substitution var ty
    }
  deriving ()

--------------------------------------------------------------------------------

-- | Safely create a 'TypedTerm'.
--
--   This ensures that:
--
--   1. The metavariable typing function is minimal (i.e.: it does not assign
--      types to any metavariables that aren't free variables of the underlying
--      term).
--   2. The result of giving the metavariable typing function to the whole-term
--      typing function is @'Just' t@ for some type @t@.
makeTypedTerm
  :: (Ord var)
  => Term node var
  -- ^ The underlying 'Term' of the 'TypedTerm' we are going to make.
  -> (Substitution var ty -> Maybe ty)
  -- ^ The overall 'Type' of the given 'Term'.
  -> Substitution var ty
  -- ^ The most general inferred type for each of the variables in the 'Term'.
  --   The function must return 'Just' if the given variable was one of the
  --   free variables of the term, or else an 'AssertionFailed' exception
  --   will be thrown before the 'TypedTerm' is returned by this function.
  -> Maybe (TypedTerm node var ty)
  -- ^ A typed term, if all the preconditions are met.
makeTypedTerm term overallType varType = do
  let free  = Term.freeVars term
  let tterm = UnsafeMkTypedTerm
              { _typedTermUnderlyingTerm = term
              , _typedTermOverallType    = overallType
              , _typedTermVarType        = \var -> if var `Set.member` free
                                                   then varType var
                                                   else Nothing
              }
  guard (all (varType .> isJust) free)
  guard (isJust (overallType varType))
  pure tterm

--------------------------------------------------------------------------------

-- | Get the underlying term of the given 'TypedTerm'.
underlyingTerm
  :: TypedTerm node var ty
  -- ^ A 'TypedTerm'.
  -> Term node var
  -- ^ The underlying term of the given 'TypedTerm'.
underlyingTerm = _typedTermUnderlyingTerm

-- | Get the whole-term typing function of the given 'TypedTerm'.
wholeTermTypingFunction
  :: TypedTerm node var ty
  -- ^ A 'TypedTerm'.
  -> (Substitution var ty -> Maybe ty)
  -- ^ The whole-term typing function of the given 'TypedTerm'.
wholeTermTypingFunction = _typedTermOverallType

-- | Get the metavariable typing function of the given 'TypedTerm'.
metavariableTypingFunction
  :: TypedTerm node var ty
  -- ^ A 'TypedTerm'.
  -> Substitution var ty
  -- ^ The metavariable typing function of the given 'TypedTerm'.
metavariableTypingFunction = _typedTermVarType

--------------------------------------------------------------------------------
