module Main where

import Control.Exception (handle)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import System.IO
import qualified Data.List as Data.Set

main :: IO ()
main = do
  print "Advent of Code 2023 day 4"
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let fileLines = lines contents

  -- printList fileLines
  --   print (stringToIntList "   12 1    1 44 323   ")
  -- print (parse (head fileLines))

  let scores = map getScoreForLine fileLines
  let fullScore = sum scores
  print fullScore

  hClose handle

getScoreForLine :: [Char] -> Int
getScoreForLine l = 
    let parsed = parse l
        matches = getMatches parsed
    in getScoreForMatches matches

getScoreForMatches :: [Int] -> Int
getScoreForMatches [] = 0
getScoreForMatches m = 2 ^ (length m -1)

-- parseLine :: [Char]-> ([Int], [Int])
parse :: [Char] -> ([Int], [Int])
parse s =
  let cardString = last (splitOn ":" s)
      cardParts = splitOn "|" cardString
      myNumbersString = head cardParts
      winningNumbersString = last cardParts
   in (stringToIntList myNumbersString, stringToIntList winningNumbersString)

getMatches :: ([Int], [Int]) -> [Int]
getMatches (mine, winning) = 
    let mySet = Set.fromList mine
        winningSet = Set.fromList winning
    in Data.Set.intersect mine winning

stringToIntList :: [Char] -> [Int]
stringToIntList s =
  let split = splitOn " " s
      filtered = filter (/= "") split
   in map (read :: [Char] -> Int) filtered

printList :: (Show a) => [a] -> IO ()
printList = mapM_ print

-- parseIntList :: [Char] -> [Int]
parseIntList :: [Char] -> [[Char]]
parseIntList s =
  let l = splitOn " " s
   in l

-- getMyNumbersSection :: [Char] -> [Char]
-- getMyNumbersSection s =
