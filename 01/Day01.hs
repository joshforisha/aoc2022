module Day01 (main) where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import System.IO

main :: IO ()
main = do
    caloriesData <- readFile "./01/input.txt"
    let elfCaloriesData = splitOn "\n\n" caloriesData
    let elfCalories = map (map (\x -> read x :: Int) . lines) elfCaloriesData
    let totals = map sum elfCalories
    let sortedTotals = sortBy (\a b -> compare b a) totals
    let [highestTotal] = take 1 sortedTotals
    putStrLn $ "Highest total: " ++ (show highestTotal)
    let top3Sum = sum (take 3 sortedTotals)
    putStrLn $ "Sum of top three: " ++ (show top3Sum)
