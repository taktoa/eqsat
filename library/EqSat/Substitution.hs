--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Substitution
  ( module EqSat.Substitution -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Arrow     (first, second)
import           Control.Exception
import           Control.Monad     (guard)
import           Data.Coerce       (coerce)
import           Data.Either
import           Data.List         (sortBy)
import           Data.Maybe
import           Data.Ord          (comparing)
import           Data.Void         (Void, absurd)

-- import qualified Data.Vector.Primitive         as PV
-- import qualified Data.Vector.Primitive.Mutable as PMV

import           Data.Vector       (Vector, (!))
import qualified Data.Vector       as Vector

import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map

import           Data.Set          (Set)
import qualified Data.Set          as Set

import           Flow              ((.>), (|>))

import qualified Hedgehog          as HH
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range

import           EqSat.Term        (TTerm, Term (MkNodeTerm, MkVarTerm))
import qualified EqSat.Term        as TTerm

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Substitution node v1 v2
  = Substitution
    { fromSubstitution :: Map v1 (TTerm node v2) }
  deriving (Eq, Ord, Show)

-- | FIXME: doc
type EndoSubstitution node var = Substitution node var var

-- | FIXME: doc
data TotalSubstitution node v1 v2
  = TotalSubstitution
    !(Substitution node v1 v2)
    !(v1 -> v2)
  deriving ()

--------------------------------------------------------------------------------

-- | FIXME: doc
endoToTotal
  :: EndoSubstitution node var
  -> TotalSubstitution node var var
endoToTotal subst = TotalSubstitution subst id

--------------------------------------------------------------------------------

-- | FIXME: doc
domain
  :: Substitution node v1 v2
  -> Set v1
domain = fromSubstitution .> Map.keysSet

-- | FIXME: doc
codomain
  :: (Ord node, Ord v1, Ord v2)
  => Substitution node v1 v2
  -> Set (TTerm node v2)
codomain = fromSubstitution .> Map.toList .> map snd .> Set.fromList

-- | FIXME: doc
introduced
  :: (Ord v2)
  => Substitution node v1 v2
  -> Set v2
introduced = fromSubstitution
             .> Map.toList
             .> map (snd .> TTerm.freeVars)
             .> mconcat

--------------------------------------------------------------------------------

-- | FIXME: doc
substituteTotal
  :: (Ord v1)
  => TotalSubstitution node v1 v2
  -> TTerm node v1
  -> TTerm node v2
substituteTotal (TotalSubstitution subst def) = go
  where
    go (MkVarTerm var)   = fromSubstitution subst
                           |> Map.lookup var
                           |> fromMaybe (MkVarTerm (def var))
    go (MkNodeTerm n cs) = MkNodeTerm n (Vector.map go cs)

-- | FIXME: doc
substituteEndo
  :: (Ord var)
  => EndoSubstitution node var
  -> TTerm node var
  -> TTerm node var
substituteEndo subst
  = substituteTotal (TotalSubstitution subst id)

--------------------------------------------------------------------------------

-- | The identity 'EndoSubstitution'.
identityEndo
  :: EndoSubstitution node var
identityEndo = Substitution Map.empty

-- |
-- Composition of 'EndoSubstitution's in the same order as the @'<.'@ operator.
--
-- Laws (abbreviating 'composeEndo' as @(∘ₑ)@):
--
-- 1. For any @s ∷ 'EndoSubstitution' n v@, @s ≡ s ∘ₑ 'identityEndo'@.
-- 2. For any @s ∷ 'EndoSubstitution' n v@, @s ≡ 'identityEndo' ∘ₑ s@.
-- 3. For any @a, b, c ∷ 'EndoSubstitution' n v@,
--    @((a ∘ₑ b) ∘ₑ c) ≡ (a ∘ₑ (b ∘ₑ c))@.
-- 4. For any @x, y ∷ 'EndoSubstitution' n v@,
--    @'substituteEndo' (y ∘ₑ x) ≡ ('substituteEndo' y '.' 'substituteEndo' x)@.
composeEndo
  :: (Ord var)
  => EndoSubstitution node var
  -> EndoSubstitution node var
  -> EndoSubstitution node var
composeEndo (Substitution s1) (Substitution s2)
  = (Map.unionWith (\_ _ -> error "lol") -- FIXME
      (Map.map (substituteEndo (Substitution s1)) s2)
      (Map.difference s1 s2))
    |> Substitution

-- | The identity 'TotalSubstitution'.
identityTotal
  :: TotalSubstitution node var var
identityTotal = TotalSubstitution identityEndo id

-- |
-- FIXME: doc
--
-- Laws (abbreviating 'composeTotal' as @(∘ₜ)@):
--
-- 1. For any @s ∷ 'TotalSubstitution' n v1 v2@, @s ≡ s ∘ₜ 'identityTotal'@.
-- 2. For any @s ∷ 'TotalSubstitution' n v1 v2@, @s ≡ 'identityTotal' ∘ₜ s@.
-- 3. For any
--    @a ∷ 'TotalSubstitution' n v3 v4@ and
--    @b ∷ 'TotalSubstitution' n v2 v3@ and
--    @c ∷ 'TotalSubstitution' n v1 v2@,
--    @((a ∘ₜ b) ∘ₜ c) ≡ (a ∘ₜ (b ∘ₜ c))@.
-- 4. For any
--    @x ∷ 'TotalSubstitution' n v1 v2@ and
--    @y ∷ 'TotalSubstitution' n v2 v3@,
--    @'substituteTotal' (y ∘ₜ x) ≡ 'substituteTotal' y '.' 'substituteTotal' x@.
composeTotal
  :: (Ord v1, Ord v2, Ord v3)
  => TotalSubstitution node v2 v3
  -> TotalSubstitution node v1 v2
  -> TotalSubstitution node v1 v3
composeTotal (ts1@(TotalSubstitution s1 def1)) (ts2@(TotalSubstitution s2 def2))
  = undefined
--   = (Map.unionWith (\_ _ -> error "lol") -- FIXME
--       (Map.map (substituteTotal ts1) (fromSubstitution s2))
--       (Map.difference
--        (Map.map (fmap _) s1)
--        (Map.map (substituteTotal ts1) (fromSubstitution s2))))
--     |> Substitution
--     |> (\s -> TotalSubstitution s (def1 . def2))

--------------------------------------------------------------------------------

-- |
-- The /join/ of two 'EndoSubstitution's, as defined
-- in /Substitution Tree Indexing/ by Peter Graf.
--
-- Laws:
--
-- 1. FIXME: laws
join
  :: (Ord var)
  => EndoSubstitution node var
  -> EndoSubstitution node var
  -> EndoSubstitution node var
join (Substitution s1) (Substitution s2)
  = (Map.unionWith (\_ _ -> error "rofl") -- FIXME
     (Map.map (substituteEndo (Substitution s1)) s2)
     ((domain (Substitution s1) `Set.difference` introduced (Substitution s2))
       |> Set.toList
       |> map (\k -> (k, fromJust (Map.lookup k s1)))
       |> Map.fromList))
    |> Substitution

--------------------------------------------------------------------------------

-- | FIXME: doc
genIdempotentEndoSubstitution
  :: forall m node var.
     (HH.MonadGen m)
  => Set var
  -- ^ The domain of the substitution.
  -> Set var
  -- ^ The variables introduced by the substitution.
  -> m (EndoSubstitution node var)
genIdempotentEndoSubstitution = undefined

-- | FIXME: doc
genEndoSubstitution
  :: forall m node var.
     (HH.MonadGen m)
  => Set var
  -- ^ The domain of the substitution.
  -> Set var
  -- ^ The variables introduced by the substitution.
  -> m (EndoSubstitution node var)
  -- ^ A random substitution.
genEndoSubstitution = undefined

-- | FIXME: doc
genGroundTotalSubstitution
  :: forall m node var.
     (HH.MonadGen m)
  => Set var
  -- ^ The domain of the substitution.
  -> m (TotalSubstitution node var Void)
  -- ^ A random ground substitution.
genGroundTotalSubstitution = undefined

-- | FIXME: doc
genTotalSubstitution
  :: forall m node v1 v2.
     (HH.MonadGen m)
  => Set v1
  -- ^ The domain of the substitution.
  -> Set v2
  -- ^ The variables introduced by the substitution.
  -> (v1 -> v2)
  -- ^ The function defining the action of the substitution on variables
  --   that are not in its domain.
  -> m (TotalSubstitution node v1 v2)
  -- ^ A random ground substitution.
genTotalSubstitution = undefined

--------------------------------------------------------------------------------
