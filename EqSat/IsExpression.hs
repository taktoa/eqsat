--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.IsExpression
  ( IsExpression (exprToTerm, termToExpr)
  ) where

--------------------------------------------------------------------------------

import           Control.Exception (SomeException)

import           EqSat.Term        (ClosedTerm)

--------------------------------------------------------------------------------

-- | FIXME: doc
class IsExpression node expr | expr -> node where
  -- | FIXME: doc
  exprToTerm :: expr -> ClosedTerm node

  -- | FIXME: doc
  termToExpr :: ClosedTerm node -> Either SomeException expr

--------------------------------------------------------------------------------

-- | Trivial instance.
instance IsExpression node (ClosedTerm node) where
  exprToTerm = id
  termToExpr = pure

--------------------------------------------------------------------------------
