{-# LANGUAGE ViewPatterns #-}

-- https://stackoverflow.com/questions/1602243/pattern-matching-string-prefixes-in-haskell

import Data.List
import Data.List.Split (splitOn)
import System.IO


main = do
  print "advent of code 2023 day 2"
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let fileLines = lines contents
  let results = map getNumberForGame fileLines
  print (sum results)
  hClose handle

-- only 12 red cubes, 13 green cubes, and 14 blue cubes?

getNumberForGame:: String -> Integer
getNumberForGame s = 
  let split = splitOn ": " s
      gameNumber = getGameNumber (head split) 
      isValid = isGameValid (last split) 
  in if isValid
        then gameNumber
      else 0

isGameValid :: String -> Bool
isGameValid s = 
  let rounds = splitOn "; " s
      isValidList = map isDrawValid rounds
  in all (==True) isValidList

isDrawValid :: String -> Bool
isDrawValid s = 
  let draws = splitOn ", " s
      isValidList = map isRollValid draws
  in all (== True) isValidList

isRollValid :: String -> Bool
isRollValid (stripSuffix " green" -> Just restOfString) =
  let number = read restOfString :: Integer
   in number <= 13
isRollValid (stripSuffix " red" -> Just restOfString) =
  let number = read restOfString :: Integer
   in number <= 12
isRollValid (stripSuffix " blue" -> Just restOfString) =
  let number = read restOfString :: Integer
   in number <= 14
isRollValid string = False

getGameNumber :: String -> Integer
getGameNumber s =
  let stripped = stripPrefix "Game " s
   in maybe 0 read stripped :: Integer

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suffix string =
  let backwardsMaybe = stripPrefix (reverse suffix) (reverse string)
   in fmap reverse backwardsMaybe

printList :: (Show a) => [a] -> IO ()
printList = mapM_ print
