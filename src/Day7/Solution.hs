module Day7.Solution
  ( fuelCost,
    median,
    solvePart1,
  )
where

import Data.List (sort)
import Flow ((|>))
import Text.Parsec (ParseError, char, digit, many1, parse, sepBy1)
import Text.Parsec.String (Parser)

fuelCost :: Int -> [Int] -> Int
fuelCost position positions = fmap cost positions |> sum
  where
    cost x = abs (position - x)

median :: [Int] -> Int
median positions =
  if odd $ length positions
    then sorted !! center
    else ((sorted !! (center - 1)) + (sorted !! center)) `div` 2
  where
    sorted = sort positions
    center = length positions `div` 2

numbers :: Parser Int
numbers = fmap read (many1 digit)

parseInputs :: Parser [Int]
parseInputs = sepBy1 numbers (char ',')

solvePart1 :: String -> Either ParseError Int
solvePart1 content = do
  inputs <- parse parseInputs "" content
  let position = median inputs
  return $ fuelCost position inputs
