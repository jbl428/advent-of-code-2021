module Day6.Solution
  ( process,
    solvePart1,
    Fish (..),
    nextDay,
    solvePart2,
  )
where

import Control.Monad (when)
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

data Fish = Fish
  { zero :: Int,
    one :: Int,
    two :: Int,
    three :: Int,
    four :: Int,
    five :: Int,
    six :: Int,
    seven :: Int,
    eight :: Int
  }
  deriving (Show, Eq)

nextDay :: Fish -> Fish
nextDay (Fish zero one two three four five six seven eight) = Fish one two three four five six (zero + seven) eight zero

toFish :: [Int] -> Fish
toFish xs =
  Fish (count 0) (count 1) (count 2) (count 3) (count 4) (count 5) (count 6) (count 7) (count 8)
  where
    count a = filter (== a) xs |> length

solvePart2 :: String -> Either ParseError Int
solvePart2 content = do
  fishs <- parse parseInputs "" content
  let (Fish a b c d e f g h i) = iterate nextDay (toFish fishs) !! 256
  return (sum [a, b, c, d, e, f, g, h, i])