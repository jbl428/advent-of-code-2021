module Day7.Solution
  ( fuelCost,
  )
where

import Flow ((|>))
import Text.Parsec (ParseError, char, digit, many1, parse, sepBy1)
import Text.Parsec.String (Parser)

fuelCost :: Int -> [Int] -> Int
fuelCost position positions = fmap cost positions |> sum
  where
    cost x = abs (position - x)

-- cheapestCost :: [Int] -> Int
-- cheapestCost positions = go (minimum positions) (maximum positions)

numbers :: Parser Int
numbers = fmap read (many1 digit)

parseInputs :: Parser [Int]
parseInputs = sepBy1 numbers (char ',')
