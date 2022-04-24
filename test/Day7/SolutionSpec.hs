module Day7.SolutionSpec where

import Day7.Solution (fuelCost, median, solvePart1)
import Test.Hspec (Spec, describe, it, shouldBe)

positions :: [Int]
positions = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

spec :: Spec
spec = do
  describe "Day 7" $ do
    it "fuleCost" $ do
      fuelCost 2 positions `shouldBe` 37
      fuelCost 1 positions `shouldBe` 41
      fuelCost 3 positions `shouldBe` 39
      fuelCost 10 positions `shouldBe` 71

    it "median" $ do
      median positions `shouldBe` 2
      median [10, 20, 80, 90] `shouldBe` 50

    it "solvePart1" $ do
      solvePart1 "16,1,2,0,4,2,7,1,2,14" `shouldBe` Right 37