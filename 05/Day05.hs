module Day05 (main) where

import Data.IntMap.Strict (IntMap)
import Data.List.Split (splitOn)
import Flow
import System.IO
import qualified Data.IntMap.Strict as IM

type Crate = Char
type Stacks = IntMap [Crate]

data Step = Step
    { count :: Int
    , source :: Int
    , destination :: Int
    } deriving (Show)

main :: IO ()
main = do
    procedureData <- readFile "./05/input.txt"
    let [stacksData, instructions] = splitOn "\n\n" procedureData |> map lines
    let startingStacks = initializeStacks stacksData
    let steps = map toStep instructions
    let singleMoveSteps = map (\(Step count src dest) -> times count (move src dest)) steps
    let part1Stacks = applyChain singleMoveSteps startingStacks
    putStrLn $ "CM9000 Crates: " ++ (part1Stacks |> IM.elems |> map last)
    let multiMoveSteps = map (\(Step count src dest) -> multiMove count src dest) steps
    let part2Stacks = applyChain multiMoveSteps startingStacks
    putStrLn $ "CM9001 Crates: " ++ (part2Stacks |> IM.elems |> map last)

applyChain :: [Stacks -> Stacks] -> Stacks -> Stacks
applyChain [] stacks = stacks
applyChain (f:fs) stacks = applyChain fs (f stacks)

divideAt :: Int -> [a] -> [[a]]
divideAt _ [] = []
divideAt num xs =
  let
    (sel, rest) = splitAt num xs
  in
    [sel] ++ (divideAt num rest)

initializeStacks :: [String] -> Stacks
initializeStacks stacksData =
  let
    rows = init stacksData
        |> map (divideAt 4)
        |> map (map (!! 1))
    initList = map (mapWithIndex (\c i -> (i + 1, [c] ++ ""))) rows
        |> concat
        |> filter (\(_, c) -> c /= " ")
  in
    IM.fromListWith (++) initList

int :: String -> Int
int x = read x :: Int

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f xs = zipWith f xs [0..]

move :: Int -> Int -> Stacks -> Stacks
move src dest stacks =
    stacks
        |> IM.adjust (\a -> init a) src
        |> IM.adjust (\b -> b ++ [last (stacks IM.! src)]) dest

multiMove :: Int -> Int -> Int -> Stacks -> Stacks
multiMove count src dest stacks =
  let
    srcCrates = stacks IM.! src
    keepNum = (length srcCrates) - count
  in
    stacks
        |> IM.adjust (\a -> take keepNum a) src
        |> IM.adjust (\b -> b ++ drop keepNum srcCrates) dest

times :: Int -> (a -> a) -> a -> a
times 0 _ a = a
times x f a = times (x - 1) f (f a)

toStep :: String -> Step
toStep instruction =
  let
    [_, count, _, source, _, destination] = words instruction
  in
    Step (int count) (int source) (int destination)
