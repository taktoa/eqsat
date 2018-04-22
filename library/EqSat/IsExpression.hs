--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.IsExpression
  ( IsExpression (exprToGTerm, gtermToExpr)
  ) where

--------------------------------------------------------------------------------

import           Control.Exception (SomeException)

import           EqSat.Term        (ClosedTerm, ReprG)

--------------------------------------------------------------------------------

-- | FIXME: doc
class IsExpression node expr | expr -> node where
  -- | FIXME: doc
  exprToGTerm :: expr -> ClosedTerm ReprG node

  -- | FIXME: doc
  gtermToExpr :: ClosedTerm ReprG node -> Either SomeException expr

--------------------------------------------------------------------------------

-- | Trivial instance.
instance IsExpression node (ClosedTerm ReprG node) where
  exprToGTerm = id
  gtermToExpr = pure

--------------------------------------------------------------------------------
