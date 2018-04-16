--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Domain
  ( Domain
  ) where

--------------------------------------------------------------------------------

import qualified Data.SBV as SBV

--------------------------------------------------------------------------------

-- | FIXME: doc
class (Num d, SBV.SymWord d, SBV.Metric d) => Domain d

-- | FIXME: doc
instance (Num d, SBV.SymWord d, SBV.Metric d) => Domain d

--------------------------------------------------------------------------------
