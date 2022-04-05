module Day6.Solution
  ( process,
    solvePart1,
  )
where

import Flow ((|>))
import Text.Parsec (ParseError, char, digit, many1, parse, sepBy1)
import Text.Parsec.String (Parser)

process :: Int -> [Int] -> [Int]
process 0 fishs = fishs
process days fishs = process (days - 1) (updatedFishs <> newFishs)
  where
    update 0 = 6
    update x = x - 1
    updatedFishs = fmap update fishs
    newFishs = filter (== 0) fishs |> length |> \s -> replicate s 8

numbers :: Parser Int
numbers = fmap read (many1 digit)

parseInputs :: Parser [Int]
parseInputs = sepBy1 numbers (char ',')

solvePart1 :: String -> Either ParseError Int
solvePart1 content = do
  fishs <- parse parseInputs "" content
  return (process 80 fishs |> length)
