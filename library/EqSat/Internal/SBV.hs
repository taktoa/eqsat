--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Internal.SBV
  ( smtConfig
  ) where

--------------------------------------------------------------------------------

import qualified Data.SBV               as SBV

import           Control.Monad.IO.Class (MonadIO (liftIO))

--------------------------------------------------------------------------------

#include "./SBVDefines.hs"

#ifndef SMT_Z3_PATH
#define SMT_Z3_PATH "z3"
#endif

--------------------------------------------------------------------------------

-- | FIXME: doc
smtConfig :: (MonadIO m) => m SBV.SMTConfig
smtConfig = do
  let solver = (SBV.solver SBV.z3)
               { SBV.executable = SMT_Z3_PATH }
  let config = SBV.z3 { SBV.solver = solver }
  liftIO (SBV.sbvCheckSolverInstallation config)
  pure config

--------------------------------------------------------------------------------
