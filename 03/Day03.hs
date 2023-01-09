module Day03 (main) where

import Data.Char (isUpper, ord)
import Data.List (nub)
import Flow
import System.IO

data Rucksack = Rucksack
    { left  :: String
    , right :: String
    } deriving (Show)

main :: IO ()
main = do
    rucksacksData <- readFile "./03/input.txt"
    let rucksacks = rucksacksData |> lines |> map splitIntoRucksack
    let sharedRucksackItems = map sharedItems rucksacks
    let sharedRucksackItemPriorities = map (\xs -> sum (map priority xs)) sharedRucksackItems
    putStrLn $ "Sum of shared item priorities: " ++ (show (sum sharedRucksackItemPriorities))
    let groups = divideIntoGroups (lines rucksacksData)
    let groupBadges = map groupBadge groups
    let groupBadgePriorities = map priority groupBadges
    putStrLn $ "Sum of group badges: " ++ (show (sum groupBadgePriorities))

divideIntoGroups :: [a] -> [[a]]
divideIntoGroups rs =
  let
    (group, rest) = splitAt 3 rs
  in
    if (length rest) > 0 then
        [group] ++ (divideIntoGroups rest)
    else
        [group]

groupBadge :: [String] -> Char
groupBadge [first, second, third] =
    filter (\c -> c `elem` second && c `elem` third) first
        |> head

priority :: Char -> Int
priority c = (ord c) - (if isUpper c then 38 else 96)

sharedItems :: Rucksack -> [Char]
sharedItems (Rucksack { left, right }) =
    filter (`elem` right) left
        |> nub

splitIntoRucksack :: String -> Rucksack
splitIntoRucksack string =
  let
    numItems = length string
  in
    Rucksack
        { left = take (numItems `div` 2) string
        , right = drop (numItems `div` 2) string
        }
