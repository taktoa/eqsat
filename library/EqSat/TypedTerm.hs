--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.TypedTerm
  ( TypedTerm
  , TypedGTerm
  , TypedTTerm
  , makeTypedTerm
  , underlyingTerm
  , wholeTermTypingFunction
  , metavariableTypingFunction
  , freeVars
  ) where

--------------------------------------------------------------------------------

import           Control.Monad   (guard)
import           Data.Maybe      (isJust)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           GHC.Generics    (Generic)

import           Flow            ((.>), (|>))

import           Data.Set        (Set)
import qualified Data.Set        as Set

import           EqSat.Term      (Term)
import qualified EqSat.Term      as Term

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
--      function @w@ and the subtyping relation @(⊆) ∷ ty → ty → 'Bool'@,
--      if @θ@ is a substitution such that for every free variable @v@ of the
--      underlying term, @((⊆) '<$>' θ v '<*>' m v) ≡ 'Just' 'True'@, then
--      there exists a type @t@ such that @w θ ≡ 'Just' t@.
data TypedTerm repr node var ty
  = UnsafeMkTypedTerm
    { _typedTermUnderlyingTerm :: Term repr node var
    , _typedTermOverallType    :: Map var ty -> Maybe ty
    , _typedTermVarType        :: Map var ty
    }
  deriving (Generic)

-- | FIXME: doc
type TypedGTerm node var ty = TypedTerm Term.ReprG node var ty

-- | FIXME: doc
type TypedTTerm node var ty = TypedTerm Term.ReprT node var ty

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
  => Term repr node var
  -- ^ The underlying 'Term' of the 'TypedTerm' we are going to make.
  -> (Map var ty -> Maybe ty)
  -- ^ The overall 'Type' of the given 'Term'.
  -> Map var ty
  -- ^ The most general inferred type for each of the variables in the 'Term'.
  --   The function must return 'Just' if the given variable was one of the
  --   free variables of the term, or else an 'AssertionFailed' exception
  --   will be thrown before the 'TypedTerm' is returned by this function.
  -> Maybe (TypedTerm repr node var ty)
  -- ^ A typed term, if all the preconditions are met.
makeTypedTerm term overallType varType = do
  let free  = Term.freeVars term
  let tterm = UnsafeMkTypedTerm
              { _typedTermUnderlyingTerm = term
              , _typedTermOverallType    = overallType
              , _typedTermVarType        = Set.toList free
                                           |> map (\v -> (v, varType Map.! v))
                                           |> Map.fromList
              }
  -- guard (all ((varType `Map.lookup`) .> isJust) free)
  guard (isJust (overallType varType))
  pure tterm

--------------------------------------------------------------------------------

-- | Get the underlying term of the given 'TypedTerm'.
underlyingTerm
  :: TypedTerm repr node var ty
  -- ^ A 'TypedTerm'.
  -> Term repr node var
  -- ^ The underlying term of the given 'TypedTerm'.
underlyingTerm = _typedTermUnderlyingTerm

-- | Get the whole-term typing function of the given 'TypedTerm'.
wholeTermTypingFunction
  :: TypedTerm repr node var ty
  -- ^ A 'TypedTerm'.
  -> (Map var ty -> Maybe ty)
  -- ^ The whole-term typing function of the given 'TypedTerm'.
wholeTermTypingFunction = _typedTermOverallType

-- | Get the metavariable typing function of the given 'TypedTerm'.
metavariableTypingFunction
  :: TypedTerm repr node var ty
  -- ^ A 'TypedTerm'.
  -> Map var ty
  -- ^ The metavariable typing function of the given 'TypedTerm'.
metavariableTypingFunction = _typedTermVarType

--------------------------------------------------------------------------------

-- | Get the 'Set' of free variables in the given 'TypedTerm'.
freeVars
  :: (Ord var)
  => TypedTerm repr node var ty
  -- ^ A typed term.
  -> Set var
  -- ^ The set of free variables in the given typed term.
freeVars = underlyingTerm .> Term.freeVars

--------------------------------------------------------------------------------
