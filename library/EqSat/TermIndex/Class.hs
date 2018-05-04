--------------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
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

import           Control.Arrow           (second)
import           Control.Monad           (mapM_)

import           Control.Monad.Primitive (PrimMonad (PrimState), RealWorld)
import           Control.Monad.ST.Strict (ST, runST)

import           Data.Hashable           (Hashable)
import           Data.Kind               (Type)

import           Data.Vector.Generic     (Vector)

import qualified Data.Vector             as BV
import qualified Data.Vector.Primitive   as PV
import qualified Data.Vector.Storable    as SV
import qualified Data.Vector.Unboxed     as UV

import           Data.Vector.Primitive   (Prim)
import           Data.Vector.Storable    (Storable)
import           Data.Vector.Unboxed     (Unbox)

import           Flow                    ((.>), (|>))

import qualified EqSat.Internal.MStack   as MStack

import           EqSat.Term              (TTerm)

--------------------------------------------------------------------------------

-- -- | FIXME: doc
-- data EquationalTheory
--   = -- | Syntactic unification.
--     Ø
--   | -- | Unification modulo associativity.
--     A
--   | -- | Unification modulo associativity and unitality.
--     AU
--   | -- | Unification modulo commutativity.
--     C
--   | -- | Unification modulo associativity and commutativity.
--     AC
--   | -- | Unification modulo associativity and commutativity and unitality.
--     ACU
--   | -- | Unification modulo idempotency.
--     I
--   | -- | Unification modulo commutativity and idempotency.
--     CI
--   | -- | Unification modulo associativity and commutativity and idempotency.
--     ACI
--   | -- | Unification modulo the axioms of a boolean ring.
--     BR
--   | -- | Unification modulo the axioms of an abelian group.
--     AG
--   deriving (Eq, Show, Read)

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
--
-- This chapter is available from one of the authors' websites for free:
-- <http://www.cs.man.ac.uk/~voronkov/papers/handbookar_termindexing.ps>.
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
    = (result :: Type)
    | result -> index

  -- -- | FIXME: doc
  -- type Theories index :: [EquationalTheory]

  {-# MINIMAL freeze, thaw, (insert | insertMany), (query | queryMany) #-}

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
  unsafeFreeze = freeze
  {-# INLINE unsafeFreeze #-}

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
  unsafeThaw = thaw
  {-# INLINE unsafeThaw #-}

  -- FIXME: perhaps use 'TreeLike' instead of 'TTerm' to avoid pointless tree
  -- transformations?

  -- | Insert a @('TTerm', value)@ pair into the given mutable term index.
  insert
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> TTerm node var
    -- ^ The 'TTerm' key to insert.
    -> value
    -- ^ The value to associate with this key.
    -> m ()
    -- ^ A 'PrimMonad' action that associates the key with the value in the
    --   mutable term index.
  insert mindex term value = do
    insertMany mindex (BV.singleton (term, value))
  {-# INLINE insert #-}

  -- | Insert a @('TTerm', value)@ pair into the given mutable term index.
  insertMany
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> BV.Vector (TTerm node var, value)
    -- ^ A vector of key-value pairs to insert.
    -> m ()
    -- ^ A 'PrimMonad' action that associates each key with its value in the
    --   mutable term index.
  insertMany mindex pairs = do
    mapM_ (uncurry (insert mindex)) pairs
  {-# INLINE insertMany #-}

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
  --       -> m ('BV.Vector' value)
  -- @
  --
  -- for two reasons.
  --
  -- Firstly, because it could be that the @value@ type has an 'UV.Unbox'
  -- instance, so it is hasty to require a (boxed) 'BV.Vector'.
  --
  -- Secondly, because it could be that your ultimate goal is to compute a
  -- hashset or other container of these values, in which case it is
  -- inefficient to allocate an intermediate 'BV.Vector'.
  --
  -- In the future, if it is found that there are term-indexing data
  -- structures for which the 'Monad' constraint here is insufficient,
  -- we may add other constraints, although I find that somewhat unlikely
  -- (in fact, I suspect that in almost all cases an 'Applicative' constraint
  -- would be sufficient).
  query
    :: (Monad m, Ord node, Hashable node, Ord var, Hashable var)
    => index node var value
    -- ^ FIXME: doc
    -> TTerm node var
    -- ^ FIXME: doc
    -> (value -> m any)
    -- ^ FIXME: doc
    -> m ()
    -- ^ FIXME: doc
  query index term callback = do
    queryMany index (BV.singleton (term, callback))
  {-# INLINE query #-}

  -- |
  -- This function is quite similar to 'query', but is available for cases
  -- where you can implement it to be more efficient than its default
  -- implementation, which is to 'freeze' the mutable term index and then
  -- call 'query' on it.
  queryMut
    :: (PrimMonad m, Key node var)
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
  {-# INLINE queryMut #-}

  -- |
  -- In some cases, it is more efficient to query a term-index data structure
  -- for a set of patterns, rather than just one.
  --
  -- This is called "many-to-many" pattern matching in the literature, and we
  -- support it by allowing an implementer to define 'queryMany' rather than
  -- 'query'. The default definition of 'query' in terms of 'queryMany' should
  -- be correct if you do this, but it will result in the unnecessary allocation
  -- of a singleton 'BV.Vector', so you may still want to define 'query' for
  -- maximum efficiency.
  queryMany
    :: (Monad m, Key node var)
    => index node var value
    -- ^ FIXME: doc
    -> BV.Vector (TTerm node var, value -> m any)
    -- ^ FIXME: doc
    -> m ()
    -- ^ FIXME: doc
  queryMany index pairs = do
    mapM_ (uncurry (query index)) pairs
  {-# INLINE queryMany #-}

  -- | FIXME: doc
  queryManyMut
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ FIXME: doc
    -> BV.Vector (TTerm node var, value -> m any)
    -- ^ FIXME: doc
    -> m ()
    -- ^ FIXME: doc
  queryManyMut mindex pairs = do
    index <- freeze mindex
    queryMany index pairs
  {-# INLINE queryManyMut #-}

-- |
-- Query an immutable term index for a @'TTerm' node var@, returning
-- an instance of 'Vector' containing all the results.
queryAndCollect
  :: (TermIndex index, Key node var, Vector vec value)
  => index node var value
  -- ^ A term index to query.
  -> TTerm node var
  -- ^ The term to query.
  -> vec value
  -- ^ An instance of 'Vector' containing a superset of the @value@s that have
  --   been inserted at that term (but a subset of all the values that have ever
  --   been inserted into that term index before it was frozen into an immutable
  --   term index).
queryAndCollect index term = runST $ do
  stack <- MStack.new
  query index term (MStack.push stack)
  MStack.freeze stack

{-# INLINE queryAndCollect #-}
{-# SPECIALIZE queryAndCollect
               :: (TermIndex index, Key node var)
               => index node var value
               -> TTerm node var
               -> BV.Vector value
               #-}
{-# SPECIALIZE queryAndCollect
               :: (TermIndex index, Key node var, Unbox value)
               => index node var value
               -> TTerm node var
               -> UV.Vector value
               #-}
{-# SPECIALIZE queryAndCollect
               :: (TermIndex index, Key node var, Prim value)
               => index node var value
               -> TTerm node var
               -> PV.Vector value
               #-}
{-# SPECIALIZE queryAndCollect
               :: (TermIndex index, Key node var, Storable value)
               => index node var value
               -> TTerm node var
               -> SV.Vector value
               #-}

-- | Query a mutable term index for a @'TTerm' node var@, returning
--   an instance of 'Vector' containing all the results.
queryAndCollectMut
  :: (PrimMonad m, TermIndex index, Key node var, Vector vec value)
  => Mut index node var value (PrimState m)
  -- ^ A mutable term index to query.
  -> TTerm node var
  -- ^ The term to search for.
  -> m (vec value)
  -- ^ A 'PrimMonad' action returning an instance of 'Vector' containing a
  --   superset of the @value@s that have been inserted at that term (but a
  --   subset of all the values that have ever been inserted into that term
  --   index before it was frozen into an immutable term index).
queryAndCollectMut index term = do
  stack <- MStack.new
  queryMut index term (MStack.push stack)
  MStack.freeze stack

{-# INLINE queryAndCollectMut #-}
{-# SPECIALIZE queryAndCollectMut
               :: (PrimMonad m, TermIndex index, Key node var)
               => Mut index node var value (PrimState m)
               -> TTerm node var
               -> m (BV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (TermIndex index, Key node var)
               => Mut index node var value RealWorld
               -> TTerm node var
               -> IO (BV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (TermIndex index, Key node var)
               => Mut index node var value s
               -> TTerm node var
               -> ST s (BV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (PrimMonad m, TermIndex index, Key node var, Unbox value)
               => Mut index node var value (PrimState m)
               -> TTerm node var
               -> m (UV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (TermIndex index, Key node var, Unbox value)
               => Mut index node var value RealWorld
               -> TTerm node var
               -> IO (UV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (TermIndex index, Key node var, Unbox value)
               => Mut index node var value s
               -> TTerm node var
               -> ST s (UV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (PrimMonad m, TermIndex index, Key node var, Prim value)
               => Mut index node var value (PrimState m)
               -> TTerm node var
               -> m (PV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (TermIndex index, Key node var, Prim value)
               => Mut index node var value RealWorld
               -> TTerm node var
               -> IO (PV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (TermIndex index, Key node var, Prim value)
               => Mut index node var value s
               -> TTerm node var
               -> ST s (PV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (PrimMonad m, TermIndex index, Key node var, Storable value)
               => Mut index node var value (PrimState m)
               -> TTerm node var
               -> m (SV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (TermIndex index, Key node var, Storable value)
               => Mut index node var value RealWorld
               -> TTerm node var
               -> IO (SV.Vector value)
               #-}
{-# SPECIALIZE queryAndCollectMut
               :: (TermIndex index, Key node var, Storable value)
               => Mut index node var value s
               -> TTerm node var
               -> ST s (SV.Vector value)
               #-}

-- |
-- Query an immutable term index for a vector of @'TTerm' node var@, returning
-- a 'BV.Vector' containing, for each 'TTerm', an instance of 'Vector'
-- containing all the results for that term.
queryAndCollectMany
  :: (TermIndex index, Key node var, Vector vec value)
  => index node var value
  -- ^ A term index to query.
  -> BV.Vector (TTerm node var)
  -- ^ The terms to search for.
  -> BV.Vector (vec value)
  -- ^ An instance of 'Vector' containing a superset of the @value@s that have
  --   been inserted at that term (but a subset of all the values that have ever
  --   been inserted into that term index before it was frozen into an immutable
  --   term index).
queryAndCollectMany index terms = runST $ do
  termStackPairs <- BV.forM terms $ \term -> do
    stack <- MStack.new
    pure (term, stack)
  queryMany index (BV.map (second MStack.push) termStackPairs)
  BV.forM termStackPairs (snd .> MStack.freeze)

-- |
-- Query an immutable term index for a vector of @'TTerm' node var@, returning
-- a 'BV.Vector' containing, for each 'TTerm', an instance of 'Vector'
-- containing all the results for that term.
queryAndCollectManyMut
  :: (PrimMonad m, TermIndex index, Key node var, Vector vec value)
  => Mut index node var value (PrimState m)
  -- ^ A term index to query.
  -> BV.Vector (TTerm node var)
  -- ^ The terms to search for.
  -> m (BV.Vector (vec value))
  -- ^ An instance of 'Vector' containing a superset of the @value@s that have
  --   been inserted at that term (but a subset of all the values that have ever
  --   been inserted into that term index before it was frozen into an immutable
  --   term index).
queryAndCollectManyMut mindex terms = do
  termStackPairs <- BV.forM terms $ \term -> do
    stack <- MStack.new
    pure (term, stack)
  queryManyMut mindex (BV.map (second MStack.push) termStackPairs)
  BV.forM termStackPairs (snd .> MStack.freeze)

--------------------------------------------------------------------------------

-- | FIXME: doc
type Key node var = (Ord node, Hashable node, Ord var, Hashable var)

--------------------------------------------------------------------------------
