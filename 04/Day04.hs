module Day04 (main) where

import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Flow
import System.IO

main :: IO ()
main = do
    rangeData <- readFile "./04/input.txt"
    let pairs = rangeData
            |> lines
            |> map (splitOn "," .> map (splitOn "-"))
            |> map (\[[a, b], [c, d]] -> (int a, int b, int c, int d))
    let fullyContainedPairs = pairs
            |> filter (\(a, b, c, d) -> a <= c && b >= d || a >= c && b <= d)
    putStrLn $ "Fully contained pairs: " ++ (show (length fullyContainedPairs))
    let overlappingPairs = pairs
            |> filter (\(a, b, c, d) -> a <= c && b >= c || c <= a && d >= a)
    putStrLn $ "Overlapping pairs: " ++ (show (length overlappingPairs))

int :: String -> Int
int x = read x :: Int
