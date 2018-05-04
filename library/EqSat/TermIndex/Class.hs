--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}

--------------------------------------------------------------------------------

-- |
-- A typeclass describing what it means to be a /term/ /index/, along with
-- functions defined in terms of this typeclass.
module EqSat.TermIndex.Class
  ( module EqSat.TermIndex.Class -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Monad           (mapM_)

import           Control.Monad.Primitive (PrimMonad (PrimState))
import           Control.Monad.ST.Strict (ST, runST)

import           Data.Hashable           (Hashable)
import           Data.Kind               (Type)

import           Data.Vector.Generic     (Vector)

import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as UV

import qualified EqSat.Internal.MStack   as MStack

import           EqSat.Term              (TTerm)

--------------------------------------------------------------------------------

-- |
-- A typeclass describing what it means to be a /term/ /index/.
--
-- A term index is essentially a data structure that allows you to compactly
-- store a set of /open/ /terms/ (rose trees with metavariable leaf nodes)
-- along with an arbitrary value for each term in such a way that once
-- constructed, the term index can be queried for a term and the query will
-- return a superset of the values associated with inserted terms that unify
-- with the query term (but a subset of the values ever inserted).
--
-- For more on term indexes, consult chapter 26 of volume 2 of
-- /The Handbook of Automated Reasoning/
-- (@doi:10.1016\/B978-044450813-3\/50028-X@).
class TermIndex (index :: Type -- the @node@ type
                       -> Type -- the @var@ type
                       -> Type -- the @value@ type
                       -> Type -- the index
                ) where
  -- |
  -- An injective type family defining the mutable version of an immutable
  -- term index. The extra @s@ parameter is for an 'ST'-style state token
  -- phantom type.
  type Mut index (node :: Type) (var :: Type) (value :: Type) (s :: Type)
    = result
    | result -> index

  --

  -- | Freeze a mutable @'Mut' index@ into an immutable @index@.
  freeze
    :: (PrimMonad m)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> m (index node var value)
    -- ^ A 'PrimMonad' instance

  -- | Thaw an immutable @index@ into a mutable @'Mut' index@.
  thaw
    :: (PrimMonad m)
    => index node var value
    -- ^ FIXME: doc
    -> m (Mut index node var value (PrimState m))
    -- ^ FIXME: doc

  -- |
  -- Unsafely freeze a mutable @'Mut' index@ into an immutable @index@.
  --
  -- This function can assume that the given mutable term index has
  -- exactly one reference in the current heap (if we had linear types,
  -- this would be a linear function).
  unsafeFreeze
    :: (PrimMonad m)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> m (index node var value)
    -- ^ A 'PrimMonad' instance

  -- |
  -- Unsafely thaw an immutable @index@ into a mutable @'Mut' index@.
  --
  -- This function can assume that the given immutable term index has
  -- exactly one reference in the current heap (if we had linear types,
  -- this would be a linear function).
  unsafeThaw
    :: (PrimMonad m)
    => index node var value
    -- ^ FIXME: doc
    -> m (Mut index node var value (PrimState m))
    -- ^ FIXME: doc

  -- FIXME: perhaps use 'TreeLike' instead of 'TTerm' to avoid pointless tree
  -- transformations?

  -- | Insert a @('TTerm', value)@ pair into the given mutable term index.
  insert
    :: (PrimMonad m, Ord node, Hashable node, Ord var, Hashable var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> TTerm node var
    -- ^ The 'TTerm' key to insert.
    -> value
    -- ^ The value to associate with this key.
    -> m ()
    -- ^ A 'PrimMonad' action that associates the key with the value in the
    --   mutable term index.

  -- |
  -- Given a term index, a 'TTerm', and a monadic callback, construct a
  -- monadic value that runs the callback on the value associated with every
  -- matching leaf node of the term index.
  --
  -- This function acts much like 'mapM_'.
  --
  -- We use this type opposed to a definition like
  --
  -- @
  -- query :: ('PrimMonad' m, … other constraints …)
  --       => index node var value
  --       -> 'TTerm' node var
  --       -> m ('V.Vector' value)
  -- @
  --
  -- for two reasons.
  --
  -- Firstly, because it could be that the @value@ type has an 'UV.Unbox'
  -- instance, so it is hasty to require a (boxed) 'V.Vector'.
  --
  -- Secondly, because it could be that your ultimate goal is to compute a
  -- hashset or other container of these values, in which case it is
  -- inefficient to allocate an intermediate 'V.Vector'.
  --
  -- In the future, if it is found that there are term-indexing data
  -- structures for which the 'Applicative' constraint here is insufficient,
  -- we may add other constraints, although I find that somewhat unlikely.
  query
    :: (Applicative m, Ord node, Hashable node, Ord var, Hashable var)
    => index node var value
    -- ^ FIXME: doc
    -> TTerm node var
    -- ^ FIXME: doc
    -> (value -> m any)
    -- ^ FIXME: doc
    -> m ()
    -- ^ FIXME: doc

  -- |
  -- This function is quite similar to 'query', but is available for cases
  -- where you can implement it to be more efficient than its default
  -- implementation, which is to 'freeze' the mutable term index and then
  -- call 'query' on it.
  queryMut
    :: (PrimMonad m, Ord node, Hashable node, Ord var, Hashable var)
    => Mut index node var value (PrimState m)
    -- ^ FIXME: doc
    -> TTerm node var
    -- ^ FIXME: doc
    -> (value -> m any)
    -- ^ FIXME: doc
    -> m ()
    -- ^ FIXME: doc
  queryMut mindex term callback = do
    index <- freeze mindex
    query index term callback

-- |
-- Query an immutable term index for a @'TTerm' node var@, returning
-- an instance of 'Vector' containing all the results.
queryAndCollect
  :: ( TermIndex index, Vector vec value
     , Ord node, Hashable node, Ord var, Hashable var
     )
  => index node var value
  -- ^ A term index to query.
  -> TTerm node var
  -- ^ The term to query.
  -> vec value
  -- ^ A 'Vector' containing a superset of the @value@s that have been inserted
  --   at that term (but a subset of all the terms that have ever been inserted
  --   into that term index before it was frozen into an immutable term index).
queryAndCollect index term = runST $ do
  stack <- MStack.new
  query index term (MStack.push stack)
  MStack.freeze stack

-- | Query a mutable term index for a @'TTerm' node var@, returning
--   an instance of 'Vector' containing all the results.
queryAndCollectMut
  :: ( PrimMonad m, TermIndex index, Vector vec value
     , Ord node, Hashable node, Ord var, Hashable var
     )
  => Mut index node var value (PrimState m)
  -- ^ FIXME: doc
  -> TTerm node var
  -- ^ FIXME: doc
  -> m (vec value)
  -- ^ FIXME: doc
queryAndCollectMut index term = do
  stack <- MStack.new
  queryMut index term (MStack.push stack)
  MStack.freeze stack

--------------------------------------------------------------------------------
