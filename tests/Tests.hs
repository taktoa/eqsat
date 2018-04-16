--------------------------------------------------------------------------------

-- | FIXME: doc
module Main
  ( main
  ) where

--------------------------------------------------------------------------------

import           Tests.Integration                      (integrationTests)
import           Tests.Unit                             (unitTests)

import qualified Test.Tasty                             as Tasty
import qualified Test.Tasty.Ingredients                 as Tasty
import qualified Test.Tasty.Ingredients.Basic           as Tasty
import qualified Test.Tasty.Ingredients.ConsoleReporter as Tasty
import qualified Test.Tasty.Options                     as Tasty
import qualified Test.Tasty.Providers                   as Tasty
import qualified Test.Tasty.Runners                     as Tasty

--------------------------------------------------------------------------------

-- | FIXME: doc
main :: IO ()
main = do
  us <- unitTests
  is <- integrationTests
  let tests = Tasty.testGroup "Tests"
              [ Tasty.testGroup "Unit tests" us
              , Tasty.testGroup "Integration tests" is
              ]
  Tasty.defaultMain tests

--------------------------------------------------------------------------------
