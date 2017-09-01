
module Main (
    main
) where

import Test.Hspec
import Test.QuickCheck

import Data.List
import Control.Monad

import Histogram

histograms :: Gen [Int]
histograms = sized $ \n -> mapM (const $ choose (0, 100)) [1..n] 

main :: IO ()
main = hspec $ do

  describe "Histogram" $ do

    it "Basic example" $ do
      histogramCorrect [6,2,5,4,5,1,6] `shouldBe` [(3, 5)]

    it "Edge case" $ do
      sort (histogramFast [2,1]) `shouldBe` sort ([(1, 1), (1, 2)])

    it "Quickcheck comparison" $
      forAll histograms $ \xs -> (not $ all (==0) xs) ==> sort (histogramCorrect xs) == sort (histogramFast xs)

