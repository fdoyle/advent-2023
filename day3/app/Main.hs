module Main where
import Data.List.Split (splitOn)
import System.IO



main :: IO ()
main = do 
    putStrLn "AoC day 3"

    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    let candidates = map getCandidateNumbers fileLines
    print candidates


getCandidateNumbers :: [Char] -> [String]
getCandidateNumbers s = 
    let list = splitOn "."  s
    in filter (/= "") list