module Day02 (main) where

import Flow
import System.IO

data Result = Draw | Lose | Win deriving (Show)
data Shape = Rock | Paper | Scissors deriving (Show)

main :: IO ()
main = do
    guideData <- readFile "./02/input.txt"
    let rounds = guideData |> lines |> map words
    let assumedPoints = rounds
            |> map (\[opp, res] -> (shape opp, shape res))
            |> map (\(oppShape, resShape) -> (shapePoints resShape) + (resultPoints (outcome oppShape resShape)))
            |> sum
    putStrLn $ "Total score (assumed): " ++ (show assumedPoints)
    let directedPoints = rounds
            |> map (\[opp, res] -> (shape opp, targetShape (result res) (shape opp)))
            |> map (\(oppShape, targetShape) -> (shapePoints targetShape) + (resultPoints (outcome oppShape targetShape)))
            |> sum
    putStrLn $ "Total score (directed): " ++ (show directedPoints)

outcome :: Shape -> Shape -> Result
outcome Rock Rock = Draw
outcome Rock Paper = Win
outcome Rock Scissors = Lose
outcome Paper Rock = Lose
outcome Paper Paper = Draw
outcome Paper Scissors = Win
outcome Scissors Rock = Win
outcome Scissors Paper = Lose
outcome Scissors Scissors = Draw

result :: String -> Result
result "X" = Lose
result "Y" = Draw
result "Z" = Win

resultPoints :: Result -> Int
resultPoints Lose = 0
resultPoints Draw = 3
resultPoints Win = 6

shape :: String -> Shape
shape "A" = Rock
shape "X" = Rock
shape "B" = Paper
shape "Y" = Paper
shape "C" = Scissors
shape "Z" = Scissors

shapePoints :: Shape -> Int
shapePoints Rock = 1
shapePoints Paper = 2
shapePoints Scissors = 3

targetShape :: Result -> Shape -> Shape
targetShape Draw shape = shape
targetShape Lose Rock = Scissors
targetShape Lose Paper = Rock
targetShape Lose Scissors = Paper
targetShape Win Rock = Paper
targetShape Win Paper = Scissors
targetShape Win Scissors = Rock
