--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Tests
  ( runTests
  , tests
  ) where

--------------------------------------------------------------------------------

import           EqSat.Tests.Integration                (integrationTests)
import           EqSat.Tests.Property                   (propertyTests)
import           EqSat.Tests.Unit                       (unitTests)

import qualified Test.Tasty                             as Tasty
import qualified Test.Tasty.Ingredients                 as Tasty
import qualified Test.Tasty.Ingredients.Basic           as Tasty
import qualified Test.Tasty.Ingredients.ConsoleReporter as Tasty
import qualified Test.Tasty.Options                     as Tasty
import qualified Test.Tasty.Providers                   as Tasty
import qualified Test.Tasty.Runners                     as Tasty

--------------------------------------------------------------------------------

-- | FIXME: doc
runTests :: IO ()
runTests = tests >>= Tasty.defaultMain

-- | FIXME: doc
tests :: IO Tasty.TestTree
tests = do
  us <- unitTests
  ps <- integrationTests
  is <- integrationTests
  let ts = [ Tasty.testGroup "Unit tests"        us
           , Tasty.testGroup "Property tests"    ps
           , Tasty.testGroup "Integration tests" is
           ]
  pure (Tasty.testGroup "Tests" ts)

--------------------------------------------------------------------------------
