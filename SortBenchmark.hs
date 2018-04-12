{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.Proxy   (Proxy (Proxy))

import qualified AdaptiveSort

main :: IO ()
main = do
  AdaptiveSort.benchmark (Proxy @Int)
