module Day15.Puzzle1 where

import Relude 
import qualified Data.IntMap.Strict as Map

puzzleInput :: [Int]
puzzleInput = [8,0,17,4,1,12]

data GameState = GameState { history :: IntMap Int, currentTurn :: Int } deriving (Show)

processInput :: [Int] -> GameState
processInput input = GameState { history = Map.fromList $ zip input [0..], currentTurn = length input - 1 }

iter :: GameState -> Int -> (GameState, Int) 
iter GameState{ history, currentTurn } lastRound = (GameState { history = newHistory, currentTurn = currentTurn + 1 }, roundValue)
  where 
    newHistory = Map.insert lastRound currentTurn history
    roundValue = ((currentTurn -) <$> Map.lookup lastRound history) & fromMaybe 0

ithNumber :: Int -> [Int] -> Maybe (GameState, Int)
ithNumber i input = turns !!? (i - length input)
  where 
    turns = [0..] & scanl (\(st, x) _ -> iter st x) (processedInput, viaNonEmpty last input & fromMaybe 0) 
    processedInput = processInput input

ans :: Maybe Int
ans = ithNumber 2020 puzzleInput <&> snd
