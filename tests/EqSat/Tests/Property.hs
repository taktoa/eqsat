--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Tests.Property
  ( propertyTests
  ) where

--------------------------------------------------------------------------------

import qualified EqSat                                  as EqSat
import           EqSat.Domain                           (Domain)
import           EqSat.Equation                         (Equation)
import qualified EqSat.Equation                         as Equation
import qualified EqSat.Internal.GraphMatching           as GraphMatching
import           EqSat.Internal.Matrix                  (Matrix)
import qualified EqSat.Internal.Matrix                  as Matrix
import           EqSat.Internal.MBitmap                 (MBitmap)
import qualified EqSat.Internal.MBitmap                 as MBitmap
import qualified EqSat.Internal.MGraph                  ()
import qualified EqSat.Internal.MGraph                  as MGraph
import           EqSat.Internal.MHashMap                (MHashMap)
import qualified EqSat.Internal.MHashMap                as MHashMap
import           EqSat.Internal.MHashSet                (MHashSet)
import qualified EqSat.Internal.MHashSet                as MHashSet
import           EqSat.IsExpression
                 (IsExpression (exprToTerm, termToExpr))
import           EqSat.Term                             (Term)
import qualified EqSat.Term                             as Term
import           EqSat.Variable                         (Variable)
import qualified EqSat.Variable                         as Variable

import           Hedgehog                               ((===))
import qualified Hedgehog                               as HH
import qualified Hedgehog.Gen                           as Gen
import qualified Hedgehog.Range                         as Range

import qualified EqSat.Internal.MBitmap.Gen             as Gen
import qualified EqSat.Internal.MGraph.Gen              as Gen
import qualified EqSat.Tests.Gen.Misc                   as Gen

import           Test.Tasty                             (TestName, TestTree)
import qualified Test.Tasty                             as Tasty
import qualified Test.Tasty.Hedgehog                    as Tasty.HH
import qualified Test.Tasty.Ingredients                 as Tasty
import qualified Test.Tasty.Ingredients.Basic           as Tasty
import qualified Test.Tasty.Ingredients.ConsoleReporter as Tasty
import qualified Test.Tasty.Options                     as Tasty
import qualified Test.Tasty.Providers                   as Tasty
import qualified Test.Tasty.Runners                     as Tasty

import           Control.Monad                          (void)
import           Control.Monad.ST.Strict                (ST, runST)

import           Control.Monad.Trans.Reader             (ReaderT)
import qualified Control.Monad.Trans.Reader             as ReaderT

import           Control.Monad.Trans.Class              (MonadTrans (lift))

import           Data.List                              (sort)

import           Data.Text                              (Text)
import qualified Data.Text                              as Text

import           Data.Vector                            (Vector)
import qualified Data.Vector                            as Vector

import qualified Data.Graph                             as Graph
import qualified Data.Tree                              as Tree

import           Data.IntMap                            (IntMap)
import qualified Data.IntMap                            as IntMap

import           Data.HashSet                           (HashSet)
import qualified Data.HashSet                           as HashSet

import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict                    as HashMap

import           Data.Proxy                             (Proxy (Proxy))

import           Flow                                   ((.>), (|>))

import           Control.Exception                      (assert)
import           System.IO                              (hPutStrLn, stderr)

--------------------------------------------------------------------------------

propMBitmapSizeUnaffected :: HH.Property
propMBitmapSizeUnaffected = do
  HH.property $ do
    action <- HH.forAll (Gen.genMBitmapActions Gen.genMBitmapAction)
    (sizeBefore, sizeAfter) <- Gen.withMBitmap $ \bitmap -> do
      !sx <- pure (MBitmap.size bitmap)
      Gen.applyMBitmapAction action bitmap
      !sy <- pure (MBitmap.size bitmap)
      pure (sx, sy)
    sizeBefore === sizeAfter

propMBitmapSetWorks :: HH.Property
propMBitmapSetWorks = do
  HH.property $ do
    index <- HH.forAll (Gen.int (Range.constant 0 maxBound))
    let genMA = HH.forAll (Gen.genMBitmapActions Gen.genMutableMBitmapAction)
    let genIA = HH.forAll (Gen.genMBitmapActions Gen.genImmutableMBitmapAction)
    action1 <- genMA
    action2 <- genIA
    action3 <- genIA
    (before, after) <- Gen.withMBitmap $ \bitmap -> do
      let i = index `mod` MBitmap.size bitmap
      Gen.applyMBitmapAction action1 bitmap
      old <- MBitmap.get bitmap i
      Gen.applyMBitmapAction action2 bitmap
      MBitmap.set bitmap i (not old)
      Gen.applyMBitmapAction action3 bitmap
      new <- MBitmap.get bitmap i
      pure (old, new)
    not before === after

propMGraphTarjanWorks :: HH.Property
propMGraphTarjanWorks = do
  HH.property $ do
    createGraph <- HH.forAll $ do
      let labelGen  = Gen.int64 (Range.constant 0 maxBound)
      let weightGen = Gen.int64 (Range.constant 0 maxBound)
      Gen.genMGraph labelGen weightGen
    (actual, truth) <- pure $ runST $ do
      g <- MGraph.newMGraph
      Gen.applyMGraphAction createGraph g
      (cg, nm, _) <- MGraph.freezeToContainersGraph (MGraph.MkSomeMGraph g)
      let nmT = HashMap.fromList (map (\(x, y) -> (y, x)) (IntMap.toList nm))
      actual <- MGraph.tarjanSCC @Vector g
                >>= (Vector.mapM
                     (\(MGraph.MkSomeMGraph gr) -> MGraph.getNodes gr))
                >>= (Vector.map (HashSet.map (nmT HashMap.!)
                                 .> HashSet.toList
                                 .> Vector.fromList)
                     .> Vector.toList .> pure)
      let truth = Graph.scc cg
                  |> map (Tree.flatten
                          .> HashSet.fromList
                          .> HashSet.toList
                          .> Vector.fromList)
      pure (sort actual, sort truth)
    actual === truth

--------------------------------------------------------------------------------

prop :: TestName -> HH.Property -> TestTree
prop = Tasty.HH.testProperty

--------------------------------------------------------------------------------

-- | Property tests for "EqSat".
test_EqSat :: IO TestTree
test_EqSat
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat" |> pure

-- | Property tests for "EqSat.Term".
test_EqSat_Term :: IO TestTree
test_EqSat_Term
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.Term" |> pure

-- | Property tests for "EqSat.Equation".
test_EqSat_Equation :: IO TestTree
test_EqSat_Equation
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.Equation" |> pure

-- | Property tests for "EqSat.Variable".
test_EqSat_Variable :: IO TestTree
test_EqSat_Variable
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.Variable" |> pure

-- | Property tests for "EqSat.Domain".
test_EqSat_Domain :: IO TestTree
test_EqSat_Domain
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.Domain" |> pure

-- | Property tests for "EqSat.IsExpression".
test_EqSat_IsExpression :: IO TestTree
test_EqSat_IsExpression
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.IsExpression" |> pure

-- | Property tests for "EqSat.Internal.MHashSet".
test_EqSat_Internal_MHashSet :: IO TestTree
test_EqSat_Internal_MHashSet
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.Internal.MHashSet" |> pure

-- | Property tests for "EqSat.Internal.MHashMap".
test_EqSat_Internal_MHashMap :: IO TestTree
test_EqSat_Internal_MHashMap
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.Internal.MHashMap" |> pure

-- | Property tests for "EqSat.Internal.MBitmap".
test_EqSat_Internal_MBitmap :: IO TestTree
test_EqSat_Internal_MBitmap
  = [ prop "propMBitmapSizeUnaffected" propMBitmapSizeUnaffected
    , prop "propMBitmapSetWorks"       propMBitmapSetWorks
      -- FIXME: write more property tests
    ] |> Tasty.testGroup "EqSat.Internal.MBitmap" |> pure

-- | Property tests for "EqSat.Internal.MGraph".
test_EqSat_Internal_MGraph :: IO TestTree
test_EqSat_Internal_MGraph
  = [ prop "propMGraphTarjanWorks" propMGraphTarjanWorks
      -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.Internal.MGraph" |> pure

-- | Property tests for "EqSat.Internal.GraphMatching".
test_EqSat_Internal_GraphMatching :: IO TestTree
test_EqSat_Internal_GraphMatching
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.Internal.GraphMatching" |> pure

-- | Property tests for "EqSat.Internal.Matrix".
test_EqSat_Internal_Matrix :: IO TestTree
test_EqSat_Internal_Matrix
  = [ -- FIXME: write property tests
    ] |> Tasty.testGroup "EqSat.Internal.Matrix" |> pure

--------------------------------------------------------------------------------

-- | FIXME: doc
propertyTests :: IO [TestTree]
propertyTests = do
  sequenceA
    [ test_EqSat
    , test_EqSat_Term
    , test_EqSat_Equation
    , test_EqSat_Variable
    , test_EqSat_Domain
    , test_EqSat_IsExpression
    , test_EqSat_Internal_MHashSet
    , test_EqSat_Internal_MHashMap
    , test_EqSat_Internal_MBitmap
    , test_EqSat_Internal_MGraph
    , test_EqSat_Internal_GraphMatching
    , test_EqSat_Internal_Matrix
    ]

--------------------------------------------------------------------------------
