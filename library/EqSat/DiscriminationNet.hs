--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module EqSat.DiscriminationNet
  ( module EqSat.DiscriminationNet -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.Primitive

import           Data.Hashable           (Hashable)

import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as HashSet

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap

import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector

import           Data.Vector.Mutable     (MVector)
import qualified Data.Vector.Mutable     as MVector

import           Data.STRef              (STRef)
import qualified Data.STRef              as STRef

import           Data.Maybe

import           Flow

import           EqSat.Term              (TTerm, Term (MkNodeTerm, MkVarTerm))
import qualified EqSat.Term              as Term

import           EqSat.Equation          (Equation)
import qualified EqSat.Equation          as Equation

import           EqSat.Internal.MHashSet (MHashSet)
import qualified EqSat.Internal.MHashSet as MHashSet

--------------------------------------------------------------------------------

data MDiscriminationNet s node var
  = MkDNNode
    !(STRef s node)
    !(MVector s (MDiscriminationNet s node var))
  | MkDNDisj
    !(MVector s (MDiscriminationNet s node var))
  | MkDNEmit
    !(MHashSet s (Equation node var))
  deriving ()

-- | FIXME: doc
new
  :: (PrimMonad m)
  => m (MDiscriminationNet (PrimState m) node var)
  -- ^ FIXME: doc
new = MkDNDisj <$> MVector.new 0



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
addRule eq = undefined -- (\dn -> fromMaybe dn (go (Equation.getLHS eq) dn))
  where
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
