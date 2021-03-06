--------------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE InstanceSigs           #-}
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
import           Control.Monad           (mapM_, void, (>=>))

import           Control.Monad.Primitive (PrimMonad (PrimState), RealWorld)
import           Control.Monad.ST.Strict (ST, runST)

import           Data.Coerce             (coerce)

import           Data.Functor.Identity   (Identity, runIdentity)
import           Data.Monoid             ((<>))

import           Data.Hashable           (Hashable)
import           Data.Kind               (Type)

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap

import           Data.Vector.Generic     (Vector)
import qualified Data.Vector.Generic     as Vector

import qualified Data.Vector             as BV
import qualified Data.Vector.Primitive   as PV
import qualified Data.Vector.Storable    as SV
import qualified Data.Vector.Unboxed     as UV

import           Data.Vector.Primitive   (Prim)
import           Data.Vector.Storable    (Storable)
import           Data.Vector.Unboxed     (Unbox)

import           Flow                    ((.>), (|>))

import           EqSat.Internal.MHashMap (MHashMap)
import qualified EqSat.Internal.MHashMap as MHashMap

import qualified EqSat.Internal.MStack   as MStack

import           EqSat.Term              (TTerm)

--------------------------------------------------------------------------------

-- |
-- An axiom in first-order equational logic with uninterpreted functions.
--
-- This is a prescribed list that can be used to build theories for which
-- equational unification is likely to be feasible / decidable.
data EquationalAxiom
  = -- | Associativity: @∀x, y, z . f(x, f(y, z)) = f(f(x, y), z)@
    Associativity
  | -- | Left unitality: @∃e . ∀x . f(e, x) = x@
    LUnitality
  | -- | Right unitality: @∃e . ∀x . f(x, e) = x@
    RUnitality
  | -- | Commutativity: @∀x, y . f(x, y) = f(y, x)@
    Commutativity
  | -- | Idempotency: @∀x . f(x, x) = x@
    Idempotency
  | -- | Invertibility:
    --   left/right unitality + @∃i . ∀x . f(i(x), x) = f(x, i(x)) = e@
    Invertibility
  deriving (Eq, Ord, Show, Read)

-- |
-- FIXME: doc
type Set a = [a]

-- |
-- An equational theory is generated by a set of axioms.
type EquationalTheory = Set EquationalAxiom

--------------------------------------------------------------------------------

-- |
-- FIXME: doc
type Key node var = (Ord node, Hashable node, Ord var, Hashable var)

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
--
-- Some of the methods in this typeclass have types like @'Monad' m ⇒ … → m a@
-- which may seem unnecessary, since @m@ can only have been constructed with
-- 'pure' (or 'return'). The reason it is written this way is so that there is
-- consistency with the mutable term index functions, which have a 'PrimMonad'
-- constraint, and so that there is forwards-compatibility in case a stronger
-- constraint is added to the 'Monad' (i.e.: if you want this to support
-- randomized data structures, you'll end up needing some kind of @MonadRandom@
-- constraint). Technically I could avoid this by adding another parameter to
-- 'TermIndex' that represents the ambient monad, but I find this to be a bit
-- less ugly for now, even though it is less general. If you want to use these
-- functions in a pure context, just use 'runIdentity' to instantiate @m@ at
-- 'Identity'.
class TermIndex (index :: Type -- the @node@ type
                       -> Type -- the @var@ type
                       -> Type -- the @value@ type
                       -> Type -- the index
                ) where
  {-# MINIMAL (new | newMut), freeze, thaw,
              (insert | insertMany), (query | queryMany) #-}

  -- |
  -- An injective type family defining the mutable version of an immutable
  -- term index. The extra @s@ parameter is for an 'ST'-style state token
  -- phantom type.
  type Mut index (node :: Type) (var :: Type) (value :: Type) (s :: Type)
    = (r :: Type)
    | r -> index node var value s

  -- |
  -- The set of equational theories that constructors in terms added to this
  -- term index can have.
  --
  -- For example, if your term index type @T ∷ ⋆ → ⋆ → ⋆ → ⋆@ has support for
  -- uninterpreted function symbols, associative function symbols, and
  -- associative-commutative function symbols, then you'd define this type
  -- family like:
  --
  -- @type 'Theories' T = \'[\'[], \'[\''Associativity'], \'[\''Associativity', \''Commutativity']]@
  type Theories index :: Set EquationalTheory

  -- |
  -- Create an empty immutable term index.
  new
    :: (Monad m, Key node var)
    => m (index node var value)
    -- ^ A monadic action returning a fresh immutable term index containing
    --   no terms.
  new = pure (runST (newMut >>= freeze))

  -- |
  -- Create an empty mutable 'TermIndex'.
  newMut
    :: (PrimMonad m, Key node var)
    => m (Mut index node var value (PrimState m))
    -- ^ A 'PrimMonad' action returning a fresh mutable term index containing
    --   no terms.
  newMut = new >>= thaw

  -- |
  -- Freeze a mutable @'Mut' index@ into an immutable @index@.
  freeze
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> m (index node var value)
    -- ^ A 'PrimMonad' action returning an immutable term index equivalent to
    --   the given mutable term index.

  -- |
  -- Thaw an immutable @index@ into a mutable @'Mut' index@.
  thaw
    :: (PrimMonad m, Key node var)
    => index node var value
    -- ^ An immutable term index.
    -> m (Mut index node var value (PrimState m))
    -- ^ A 'PrimMonad' action returning a mutable term index equivalent to
    --   the given immutable term index.

  -- |
  -- Unsafely freeze a mutable @'Mut' index@ into an immutable @index@.
  --
  -- This function can assume that the given mutable term index has
  -- exactly one reference in the current heap (if we had linear types,
  -- this would be a linear function).
  unsafeFreeze
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> m (index node var value)
    -- ^ A 'PrimMonad' action returning an immutable term index equivalent to
    --   the given mutable term index.
  unsafeFreeze = freeze
  {-# INLINE unsafeFreeze #-}

  -- |
  -- Unsafely thaw an immutable @index@ into a mutable @'Mut' index@.
  --
  -- This function can assume that the given immutable term index has
  -- exactly one reference in the current heap (if we had linear types,
  -- this would be a linear function).
  unsafeThaw
    :: (PrimMonad m, Key node var)
    => index node var value
    -- ^ An immutable term index
    -> m (Mut index node var value (PrimState m))
    -- ^ A 'PrimMonad' action returning a mutable term index equivalent to
    --   the given immutable term index.
  unsafeThaw = thaw
  {-# INLINE unsafeThaw #-}

  -- FIXME: perhaps use 'TreeLike' instead of 'TTerm' to avoid pointless tree
  -- transformations?

  -- |
  -- Insert a @('TTerm', value)@ pair into the given mutable term index.
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

  -- |
  -- Insert a @('TTerm', value)@ pair into the given mutable term index.
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
  -- query ∷ ('PrimMonad' m, … other constraints …)
  --       ⇒ index node var value
  --       → 'TTerm' node var
  --       → m ('BV.Vector' value)
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
    :: (Monad m, Key node var)
    => index node var value
    -- ^ An immutable term index to query.
    -> TTerm node var
    -- ^ The query term.
    -> (value -> m void)
    -- ^ The callback to run on each @value@ associated with an inserted term
    --   that unifies with the query term.
    -> m ()
    -- ^ The monadic action resulting from the sequencing, in arbitrary order,
    --   of the return values of all the callback executions.
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
    -- ^ A mutable term index to query.
    -> TTerm node var
    -- ^ The query term.
    -> (value -> m void)
    -- ^ The callback to run on each @value@ associated with an inserted term
    --   that unifies with the query term.
    -> m ()
    -- ^ The 'PrimMonad' action resulting from the sequencing, in arbitrary
    --   order, of the return values of all the callback executions.
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
    -- ^ An immutable term index to query.
    -> BV.Vector (TTerm node var, value -> m void)
    -- ^ A 'BV.Vector' of query terms and their associated callbacks.
    -> m ()
    -- ^ A monadic action resulting from the sequencing, in arbitrary order,
    --   of the return values of all the callback executions.
  queryMany index pairs = do
    mapM_ (uncurry (query index)) pairs
  {-# INLINE queryMany #-}

  -- |
  -- This function is quite similar to 'queryMany', but is available for cases
  -- where you can implement it to be more efficient than its default
  -- implementation, which is to 'freeze' the mutable term index and then
  -- call 'queryMany' on it.
  queryManyMut
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index to query.
    -> BV.Vector (TTerm node var, value -> m void)
    -- ^ A 'BV.Vector' of query terms and their associated callbacks.
    -> m ()
    -- ^ A 'PrimMonad' action resulting from the sequencing, in arbitrary order,
    --   of the return values of all the callback executions.
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
  -- ^ An immutable term index to query.
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

-- |
-- Query a mutable term index for a @'TTerm' node var@, returning
-- an instance of 'Vector' containing all the results.
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
  -- ^ An immutable term index to query.
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
  -- ^ An immutable term index to query.
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

-- |
-- A /perfect/ term index is a term index that, when queried on a given 'TTerm',
-- returns /exactly/ the set of inserted terms (and the associated values) that
-- unify with the query term.
--
-- I can't think of a way to make the methods of this typeclass encapsulate this
-- property, but certainly the ability to implement 'forIndex' is an indicator
-- that there is a one-to-one relationship between terms and their associated
-- values, and it may prove useful in writing a law for this typeclass.
--
-- Laws:
--
-- 1. @('new' '>>=' 'flip' 'forIndex' cb) ≡ 'pure' ()@.
-- 2. @'forIndexMut' m cb ≡ ('freeze' m '>>=' 'flip' 'forIndex')@.
-- 3. FIXME: more laws
class (TermIndex index) => Perfect index where
  {-# MINIMAL forIndex #-}

  -- |
  -- Run the given monadic callback on every term-value pair that has ever been
  -- inserted into the given immutable term index.
  forIndex
    :: (Monad m, Key node var)
    => index node var value
    -- ^ An immutable term index.
    -> (TTerm node var -> value -> m void)
    -- ^ A monadic callback that takes a term-value pair.
    -> m ()
    -- ^ A monadic action resulting from the sequencing, in arbitrary order,
    --   of the result of calling the given monadic callback on every term-value
    --   pair that has been inserted into the given immutable term index.

  -- |
  -- Run the given monadic callback on every term-value pair that has ever been
  -- inserted into the given mutable term index.
  forIndexMut
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> (TTerm node var -> value -> m void)
    -- ^ A monadic callback that takes a term-value pair.
    -> m ()
    -- ^ A 'PrimMonad' action resulting from the sequencing, in arbitrary order,
    --   of the result of calling the given monadic callback on every term-value
    --   pair that has been inserted into the given mutable term index.
  forIndexMut mindex cb = do
    index <- freeze mindex
    forIndex index cb

--------------------------------------------------------------------------------

-- |
-- A /mergeable/ term index.
--
-- FIXME: doc
--
-- Laws:
--
-- 1. @'merge' 'BV.empty' ≡ 'runIdentity' 'new'@.
-- 2. @'merge' is ≡ 'merge' ('BV.sort' is)@.
-- 3. @'runST' ('thaw' i '>>=' (\\m → 'append' m is '>>' 'freeze' m)) ≡ 'merge' ('BV.singleton' i '<>' is)@.
-- 4. @'appendMut' m ms ≡ ('BV.mapM' 'freeze' ms '>>=' 'append' m)@.
-- 5. FIXME: more laws
class (TermIndex index) => Mergeable index where
  {-# MINIMAL append | appendMut #-}

  -- |
  -- Given a 'BV.Vector' of immutable term indexes, this function will return a
  -- single immutable term index representing the union of their contents.
  --
  -- Note that there is no need for this to take an associative "combining"
  -- function @f ∷ value → value → value@ /à la/ 'HashMap.insertWith',
  -- because the implicit semantics of a term index is that queries may return
  -- arbitrarily many unique already-inserted @value@s. In other words, the
  -- simplest example of a term index is
  -- @'HashMap' ('TTerm' node var) ('BV.Vector' value)@, rather than a simple
  -- @'HashMap' ('TTerm' node var) value@, and since there is already a natural
  -- 'Semigroup' structure on 'BV.Vector', we do not need to ask the user to
  -- provide a combining function.
  merge
    :: (Key node var)
    => BV.Vector (index node var value)
    -- ^ A 'BV.Vector' of immutable term indexes to combine.
    -> index node var value
    -- ^ An immutable term index equivalent to the result of running all the
    --   'insert' operations that have been run on the given term indexes on
    --   a fresh term index.
  merge indices = runST $ do
    if BV.null indices
      then new
      else do i <- thaw (BV.head indices)
              append i (BV.tail indices)
              freeze i
  {-# INLINE merge #-}

  -- |
  -- FIXME: doc
  --
  -- See the documentation for 'merge' to understand why this function does not
  -- take an extra argument of type @value → value → value@.
  append
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index into which the contents of the given immutable
    --   term indices will be added.
    -> BV.Vector (index node var value)
    -- ^ A 'BV.Vector' of immutable term indices.
    -> m ()
    -- ^ A 'PrimMonad' action that mutates the given mutable term index.
  append mindex indices = do
    mindices <- BV.mapM thaw indices
    appendMut mindex mindices
  {-# INLINE append #-}

  -- |
  -- FIXME: doc
  --
  -- See the documentation for 'merge' to understand why this function does not
  -- take an extra argument of type @value → value → value@.
  appendMut
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index into which the contents of the given mutable
    --   term indices will be added.
    -> BV.Vector (Mut index node var value (PrimState m))
    -- ^ A 'BV.Vector' of mutable term indices, which will be used in a
    --   read-only way by this function.
    -> m ()
    -- ^ A 'PrimMonad' action that mutates the given mutable term index.
  appendMut mindex mindices = do
    indices <- BV.mapM freeze mindices
    append mindex indices
  {-# INLINE appendMut #-}

--------------------------------------------------------------------------------

-- |
-- A typeclass for term indices that can have inserted terms removed.
--
-- Laws:
--
-- 1. @'remove' i t cb ≡ 'removeMany' i ('BV.singleton' (t, cb))@.
-- 2. @'removeMut' i t cb ≡ 'removeManyMut' i ('BV.singleton' (t, cb))@.
-- 3. @'removeMany' i ps ≡ 'BV.foldr' ('>=>') 'pure' ('BV.map' (\\(t, cb) i -> 'remove' i t cb) ps) i@.
-- 4. @'removeManyMut' i ps ≡ 'BV.mapM_' ('uncurry' ('removeMut' i)) ps@.
class (TermIndex index) => Removeable index where
  -- Use definition below once https://github.com/haskell/haddock/issues/834
  -- is fixed and we are on a version of GHC / Haddock with the fix:
  -- {-# MINIMAL (remove | removeMany), (removeMut | removeManyMut) #-}
  {-# MINIMAL   remove,     (removeMut | removeManyMut)
              | removeMany, (removeMut | removeManyMut) #-}

  -- |
  -- Remove the given term from the given immutable term index.
  -- The given monadic callback will be called in arbitrary order on each value
  -- that has been associated with the given term in the given term index.
  remove
    :: (Monad m, Key node var)
    => index node var value
    -- ^ An immutable term index.
    -> TTerm node var
    -- ^ A term to remove from the given immutable term index.
    -> (value -> m void)
    -- ^ A callback that can access the value associated with this term before
    --   it is deleted (it will be called once for each value).
    -> m (index node var value)
    -- ^ A monadic action resulting from the sequencing (in arbitrary order) of
    --   the monadic actions returned by the given callback, when called on each
    --   value associated with the given term. It returns a version of the
    --   immutable term index that does not contain the given term.
  remove index term callback = do
    removeMany index (BV.singleton (term, callback))
  {-# INLINE remove #-}

  -- |
  -- Given a list of terms and monadic callbacks, remove each term from the
  -- given immutable term index and call the monadic callback on each value
  -- that has been associated with the term in the given term index.
  removeMany
    :: (Monad m, Key node var)
    => index node var value
    -- ^ An immutable term index.
    -> BV.Vector (TTerm node var, value -> m void)
    -- ^ A 'BV.Vector' of pairs of terms and callbacks. Each term will be
    --   removed from the given immutable term index, and any values associated
    --   with that term will be given to the callback.
    -> m (index node var value)
    -- ^ A monadic action resulting from the sequencing (in arbitrary order) of
    --   the monadic actions returned by the given callback, when called on each
    --   value associated with the given term. It returns a version of the
    --   immutable term index that does not contain the given term.
  removeMany index pairs = do
    let composeKleisli :: (Monad m) => BV.Vector (a -> m a) -> a -> m a
        composeKleisli = BV.foldr (>=>) pure
    BV.map (\(t, cb) i -> remove i t cb) pairs
      |> composeKleisli
      |> (\f -> f index)
  {-# INLINE removeMany #-}

  -- |
  -- Remove the given term from the given mutable term index.
  -- The given monadic callback will be called in arbitrary order on each value
  -- that has been associated with the given term in the given term index.
  removeMut
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> TTerm node var
    -- ^ A term to remove from the given mutable term index.
    -> (value -> m void)
    -- ^ A callback that can access the value associated with this term before
    --   it is deleted (it will be called once for each value).
    -> m ()
    -- ^ A 'PrimMonad' action that will mutate the given mutable term index so
    --   that it no longer contains the given term. Additionally, any values
    --   associated with this term will have the given callback called on them.
  removeMut indexMut term callback = do
    removeManyMut indexMut (BV.singleton (term, callback))
  {-# INLINE removeMut #-}

  -- |
  -- Given a list of terms and monadic callbacks, remove each term from the
  -- given mutable term index and call the monadic callback on each value
  -- that has been associated with the term in the given term index.
  removeManyMut
    :: (PrimMonad m, Key node var)
    => Mut index node var value (PrimState m)
    -- ^ A mutable term index.
    -> BV.Vector (TTerm node var, value -> m void)
    -- ^ A 'BV.Vector' of pairs of terms and callbacks. Each term will be
    --   removed from the given mutable term index, and any values associated
    --   with that term will be given to the callback.
    -> m ()
    -- ^ A 'PrimMonad' action that will mutate the given mutable term index so
    --   that it no longer contains any of the terms in the given 'BV.Vector'.
    --   Additionally, for each term-callback pair in the given 'BV.Vector',
    --   any values associated with the term will have the callback called on
    --   them.
  removeManyMut indexMut pairs = do
    BV.mapM_ (uncurry (removeMut indexMut)) pairs
  {-# INLINE removeManyMut #-}

--------------------------------------------------------------------------------

-- |
-- FIXME: doc
newtype TrivialIndex node var value
  = MkTrivialIndex (HashMap (TTerm node var) (BV.Vector value))
  deriving ()

-- |
-- FIXME: doc
newtype MTrivialIndex node var value s
  = MkMTrivialIndex (MHashMap s (TTerm node var) (BV.Vector value))
  deriving ()

-- |
-- FIXME: doc
instance TermIndex TrivialIndex where
  type Mut TrivialIndex node var value s
    = MTrivialIndex node var value s

  type Theories TrivialIndex
    = '[ '[] ]

  new
    :: (Monad m, Key node var)
    => m (TrivialIndex node var value)
  new = pure (MkTrivialIndex HashMap.empty)

  freeze
    :: (PrimMonad m, Key node var)
    => MTrivialIndex node var value (PrimState m)
    -> m (TrivialIndex node var value)
  freeze (MkMTrivialIndex mhm) = do
    hm <- MHashMap.freeze mhm
    pure (MkTrivialIndex hm)

  thaw
    :: (PrimMonad m, Key node var)
    => TrivialIndex node var value
    -> m (MTrivialIndex node var value (PrimState m))
  thaw (MkTrivialIndex hm) = do
    mhm <- MHashMap.thaw hm
    pure (MkMTrivialIndex mhm)

  insert
    :: (PrimMonad m, Key node var)
    => MTrivialIndex node var value (PrimState m)
    -> TTerm node var
    -> value
    -> m ()
  insert (MkMTrivialIndex mhm) term value = do
    -- FIXME: this seems potentially slower than just using a list?
    MHashMap.insertWith mhm term (BV.singleton value) (\a b -> pure (a <> b))

  query
    :: (Monad m, Key node var)
    => TrivialIndex node var value
    -> TTerm node var
    -> (value -> m void)
    -> m ()
  query (MkTrivialIndex hm) term cb = do
    case HashMap.lookup term hm of
      Just values -> Vector.mapM_ cb values
      Nothing     -> pure ()

-- |
-- FIXME: doc
instance Perfect TrivialIndex where
  forIndex
    :: (Monad m, Key node var)
    => TrivialIndex node var value
    -> (TTerm node var -> value -> m void)
    -> m ()
  forIndex (MkTrivialIndex hm) cb = do
    HashMap.toList hm
      |> map (\(term, vals) -> BV.mapM_ (cb term .> void) vals)
      |> sequence_

--------------------------------------------------------------------------------
