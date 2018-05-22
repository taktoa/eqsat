--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Substitution
  ( module EqSat.Substitution -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           EqSat.Term (Term)
import qualified EqSat.Term as Term

--------------------------------------------------------------------------------

-- | A substitution is basically a partial function.
--
--   We use a function type rather than a 'HashMap' or 'Map' because we want to
--   allow users to use whatever dictionary type makes sense for their
--   application, as long as it supports lookup.
type Substitution a b
  = a -> Maybe b

--------------------------------------------------------------------------------
