--------------------------------------------------------------------------------

-- | FIXME: doc
module Main
  ( main
  ) where

--------------------------------------------------------------------------------

import           Tests.Integration (integrationTests)
import           Tests.Unit        (unitTests)

--------------------------------------------------------------------------------

-- | FIXME: doc
main :: IO ()
main = do
  unitTests
  integrationTests

--------------------------------------------------------------------------------
