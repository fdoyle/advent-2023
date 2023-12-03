import Control.Monad
import Data.Char
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.IO

replace from to = intercalate to . splitOn from

main = do
  print "advent of code 2023 day 1"
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let fileLines = lines contents
  let replaced = map replaceall fileLines
  let digits = map getDigits replaced
  let codes = map getCodeForLine digits
  let codeSum = sum codes
  putStrLn $ "sum: " ++ show codeSum
  hClose handle


--this is so dumb i cant believe it works
--by adding the first and last letters to the replaced string, it doesn't interfere with words that blend from one to the other
--by default, replacement consumes the text and replaces it with the digit. this replaces it with the digit, plus enough characters
--for the next word to work, but not so many characters that it can hallucinate a word that wouldn't have been there
replaceall =
  replace "one" "o1e"
    . replace "two" "t2o"
    . replace "three" "3e"
    . replace "four" "4r"
    . replace "five" "5e"
    . replace "six" "6x"
    . replace "seven" "7n"
    . replace "eight" "e8t"
    . replace "nine" "9e"

getDigits :: String -> String
getDigits s = filter isDigit s

getCodeForLine :: String -> Int
getCodeForLine s = read [head s, last s] :: Int

plusOne :: Int -> Int
plusOne x = x + 1

-- filter line to numerals

-- get first digit
-- get last digit

-- combine both

-- sum all of them