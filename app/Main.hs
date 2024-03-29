module Main where

import qualified Day1.Solution as D1
import qualified Day2.Solution as D2
import qualified Day3.Solution as D3
import qualified Day4.Solution as D4
import qualified Day5.Solution as D5
import qualified Day6.Solution as D6
import qualified Day7.Solution as D7

main :: IO ()
main = do
  content <- readFile "input/day1.txt"
  print $ D1.solve content
  content <- readFile "input/day2.txt"
  print $ D2.solve content
  content <- readFile "input/day3.txt"
  print $ D3.solvePart1 content
  print $ D3.solvePart2 content
  content <- readFile "input/day4.txt"
  print $ D4.solvePart1 content
  print $ D4.solvePart2 content
  content <- readFile "input/day5.txt"
  print $ D5.solvePart1 content
  print $ D5.solvePart2 content
  content <- readFile "input/day6.txt"
  print $ D6.solvePart1 content
  print $ D6.solvePart2 content
  content <- readFile "input/day7.txt"
  print $ D7.solvePart1 content
