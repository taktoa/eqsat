--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeInType     #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.TermIndex.Class
  ( module EqSat.TermIndex.Class -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.Primitive (PrimMonad (PrimState))
import           Data.Hashable           (Hashable)
import           Data.Kind               (Type)

import           EqSat.Term              (TTerm)

--------------------------------------------------------------------------------

-- | FIXME: doc
class TermIndex (index :: Type -> Type -> Type -> Type) where
  -- | FIXME: doc
  data Mut index
    :: Type -- the @node@ type
    -> Type -- the @var@ type
    -> Type -- the @value@ type
    -> Type -- the @s@ / @PrimState@ type
    -> Type -- the mutable version of the index

  -- | FIXME: doc
  freeze
    :: (PrimMonad m)
    => Mut index node var value (PrimState m)
    -- ^ FIXME: doc
    -> m (index node var value)
    -- ^ FIXME: doc

  -- | FIXME: doc
  thaw
    :: (PrimMonad m)
    => index node var value
    -- ^ FIXME: doc
    -> m (Mut index node var value (PrimState m))
    -- ^ FIXME: doc

  -- FIXME: perhaps use 'TreeLike' instead of 'TTerm' to avoid pointless tree
  -- transformations?

  -- | FIXME: doc
  insert
    :: (PrimMonad m, Ord node, Hashable node, Ord var, Hashable var)
    => TTerm node var
    -- ^ FIXME: doc
    -> Mut index node var value (PrimState m)
    -- ^ FIXME: doc
    -> m ()
    -- ^ FIXME: doc

  -- | FIXME: doc
  query
    :: (Monad m)
    => index node var value
    -- ^ FIXME: doc
    -> TTerm node var
    -- ^ FIXME: doc
    -> (value -> m any)
    -- ^ FIXME: doc
    -> m ()
    -- ^ FIXME: doc

--------------------------------------------------------------------------------
