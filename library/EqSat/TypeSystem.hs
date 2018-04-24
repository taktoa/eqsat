--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.TypeSystem
  ( TypeSystem ( Type, TypeError
               , inferType, isSubtype
               , showTypeError, showTypeErrorANSI
               )
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.Error.Class    (MonadError)

import           Data.Hashable                (Hashable)

import           Data.HashMap.Strict          (HashMap)
import           Data.Map.Strict              (Map)
import           Data.Proxy                   (Proxy)

import           EqSat.IsExpression           (IsExpression)
import           EqSat.Term                   (Term)
import           EqSat.TypedTerm              (TypedTerm)

import qualified EqSat.Internal.PrettyPrinter as PP

--------------------------------------------------------------------------------

-- | FIXME: doc
class (IsExpression node expr) => TypeSystem node expr where
  -- | A type whose values represent the types of expressions.
  data Type expr

  -- | A type whose values represent type errors.
  data TypeError expr

  -- | Infer the type of an open term.
  --
  --   Takes as input a proxy value (e.g.: 'Proxy') to avoid use of
  --   @-XAllowAmbiguousTypes@ as well as an open 'Term' with variables
  --   in an arbitrary @var@ type.
  --
  --   Returns a @'Maybe' ('TypedTerm' repr var node expr)@.
  --   'Nothing' should only be returned in case of a failure in type inference.
  --
  --   The 'Ord' and 'Hashable' instances are there so that you can efficiently
  --   create a 'TypedTerm' with a 'Map' or a 'HashMap' for the types of the
  --   variables.
  inferType
    :: (Ord var, Hashable var, MonadError (TypeError expr) m)
    => Term repr node var
    -> m (TypedTerm repr node var (Type expr))

  -- | Return 'True' if the first given 'Type' is a subtype of the second
  --   given 'Type' in your type system, and return 'False' otherwise.
  --
  --   This function takes as input a proxy value (e.g.: 'Proxy') to avoid use
  --   of @-XAllowAmbiguousTypes@.
  --
  --   Laws:
  --
  --   1. This should be a total function.
  --   2. This should be a preorder (transitive and reflexive).
  isSubtype
    :: Type expr
    -> Type expr
    -> Bool

  -- FIXME: maybe `isSubtype` should have type
  --        `(MonadError (TypeError expr) m) => Type expr -> Type expr -> m ()`.

  -- | Pretty-print a @'TypeError' expr@ as a 'PP.Doc'.
  --   This version does not allow any annotations.
  showTypeError
    :: TypeError expr
    -> PP.Doc ann

  -- | Pretty-print a @'TypeError' expr@ as a 'PP.Doc'.
  --   This version allows you to add ANSI color annotations, if you want.
  --   If you don't want to add ANSI coloring, there is a default definition
  --   in terms of 'showTypeError'.
  --
  --   Laws:
  --
  --   1. For any @e ∷ 'TypeError' expr@ and @p ∷ 'Proxy' expr@,
  --      @(\\_ → ()) '<$>' 'showTypeErrorANSI' p e ≡ 'showTypeError' p e@.
  showTypeErrorANSI
    :: TypeError expr
    -> PP.Doc PP.AnsiStyle
  showTypeErrorANSI = showTypeError

--------------------------------------------------------------------------------
