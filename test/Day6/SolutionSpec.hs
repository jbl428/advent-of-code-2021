module Day6.SolutionSpec where

import Day6.Solution (process, solvePart1)
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
