--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

--------------------------------------------------------------------------------

module Tests.Gen.MBitmap
  ( MBitmapAction
  , makeMBitmapAction
  , emptyMBitmapAction
  , composeMBitmapAction
  , applyMBitmapAction
  , genImmutableMBitmapAction
  , genMutableMBitmapAction
  , genMBitmapAction
  , genMBitmapActions
  , withMBitmap
  ) where

--------------------------------------------------------------------------------

import           EqSat.Internal.MBitmap  (MBitmap)
import qualified EqSat.Internal.MBitmap  as MBitmap

import qualified Hedgehog                as HH
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range
import qualified Tests.Gen.Misc          as Gen

import           Control.Monad.ST.Strict (ST, runST)

import           Data.Text               (Text)

import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector

import           Data.Proxy              (Proxy (Proxy))

import           Flow                    ((.>), (|>))

--------------------------------------------------------------------------------

data MBitmapAction
  = MkMBitmapAction
    { _MBitmapAction_names  :: !(Vector Text)
    , _MBitmapAction_action :: !(forall s. MBitmap s -> ST s ())
    }
  deriving ()

instance Show MBitmapAction where
  show = _MBitmapAction_names .> show .> (\x -> "(MBitmapAction " ++ x ++ ")")

makeMBitmapAction :: Text -> (forall s. MBitmap s -> ST s ()) -> MBitmapAction
makeMBitmapAction name action = MkMBitmapAction (Vector.singleton name) action

emptyMBitmapAction :: MBitmapAction
emptyMBitmapAction
  = MkMBitmapAction
    { _MBitmapAction_names  = Vector.empty
    , _MBitmapAction_action = (\_ -> pure ())
    }

composeMBitmapAction :: MBitmapAction -> MBitmapAction -> MBitmapAction
composeMBitmapAction actionA actionB
  = MkMBitmapAction
    { _MBitmapAction_names  = [ _MBitmapAction_names actionA
                              , _MBitmapAction_names actionB
                              ] |> mconcat
    , _MBitmapAction_action = (\bitmap -> do
                                  _MBitmapAction_action actionA bitmap
                                  _MBitmapAction_action actionB bitmap)
    }

applyMBitmapAction :: MBitmapAction
                   -> MBitmap s
                   -> ST s ()
applyMBitmapAction = _MBitmapAction_action

--------------------------------------------------------------------------------

genImmutableMBitmapAction
  :: (HH.MonadGen m)
  => m MBitmapAction
genImmutableMBitmapAction = do
  Gen.choice
    [ do size <- Gen.int (Range.linear 1 128)
         pure $ makeMBitmapAction "MBitmap.new" $ \_ -> do
           bitmap <- MBitmap.new size
           pure (bitmap `seq` ())
    , do pure $ makeMBitmapAction "MBitmap.size" $ \bitmap -> do
           size <- pure (MBitmap.size bitmap)
           pure (size `seq` ())
    , do index <- Gen.int (Range.constant 0 maxBound)
         pure $ makeMBitmapAction "MBitmap.get" $ \bitmap -> do
           let i = index `mod` MBitmap.size bitmap
           bool <- MBitmap.get bitmap i
           pure (bool `seq` ())
    , do pure $ makeMBitmapAction "MBitmap.isAllTrue" $ \bitmap -> do
           bool <- MBitmap.isAllTrue bitmap
           pure (bool `seq` ())
    , do pure $ makeMBitmapAction "MBitmap.isAllFalse" $ \bitmap -> do
           bool <- MBitmap.isAllFalse bitmap
           pure (bool `seq` ())
    , do pure $ makeMBitmapAction "MBitmap.freezeToUVector" $ \bitmap -> do
           vector <- MBitmap.freezeToUVector bitmap
           pure (vector `seq` ())
    , do vector <- Gen.vector Proxy (Range.linear 1 128) Gen.bool
         pure $ makeMBitmapAction "MBitmap.thawFromUVector" $ \_ -> do
           bitmap <- MBitmap.thawFromUVector vector
           pure (bitmap `seq` ())
    ]

genMutableMBitmapAction :: (HH.MonadGen m) => m MBitmapAction
genMutableMBitmapAction = do
  Gen.choice
    [ do index <- Gen.int (Range.constant 0 maxBound)
         value <- Gen.bool
         pure $ makeMBitmapAction "MBitmap.set" $ \bitmap -> do
           let i = index `mod` MBitmap.size bitmap
           MBitmap.set bitmap i value
    , do value <- Gen.bool
         pure $ makeMBitmapAction "MBitmap.fill" $ \bitmap -> do
           MBitmap.fill bitmap value
    ]

genMBitmapAction :: (HH.MonadGen m) => m MBitmapAction
genMBitmapAction = do
  Gen.choice
    [ genImmutableMBitmapAction
    , genMutableMBitmapAction
    ]

genMBitmapActions :: (HH.MonadGen m) => m MBitmapAction -> m MBitmapAction
genMBitmapActions underlyingGen = do
  let go depth = Gen.choice
                 [ underlyingGen
                 , if depth <= 0
                   then pure emptyMBitmapAction
                   else do !actionA <- go (depth - 1)
                           !actionB <- go (depth - 1)
                           pure (composeMBitmapAction actionA actionB)
                 ]
  Gen.int (Range.linear 0 20) >>= go

withMBitmap :: (forall s. MBitmap s -> ST s a) -> HH.PropertyT IO a
withMBitmap callback = do
  size <- HH.forAll (Gen.int (Range.linear 1 128))
  pure $ runST (MBitmap.new size >>= callback)

--------------------------------------------------------------------------------
