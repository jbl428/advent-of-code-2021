module Day6.SolutionSpec where

import Day6.Solution (Fish (..), nextDay, process, solvePart1, solvePart2)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Day 6" $ do
    it "process" $ do
      process 1 [3, 4, 3, 1, 2] `shouldBe` [2, 3, 2, 0, 1]
      process 2 [3, 4, 3, 1, 2] `shouldBe` [1, 2, 1, 6, 0, 8]
      process 10 [3, 4, 3, 1, 2] `shouldBe` [0, 1, 0, 5, 6, 0, 1, 2, 2, 3, 7, 8]

    it "solvePart1" $ do
      solvePart1 "3,4,3,1,2" `shouldBe` Right 5934

    it "nextDay" $ do
      nextDay (Fish 0 1 1 2 1 0 0 0 0) `shouldBe` Fish 1 1 2 1 0 0 0 0 0
      nextDay (Fish 3 2 2 1 0 1 1 1 1) `shouldBe` Fish 2 2 1 0 1 1 4 1 3

    it "solvePart2" $ do
      solvePart2 "3,4,3,1,2" `shouldBe` Right 26984457539
