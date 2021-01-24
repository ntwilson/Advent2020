{-# LANGUAGE BangPatterns #-}

module Day15.Puzzle2 where

import Relude 
import qualified Data.IntMap.Strict as Map
import Data.HashTable.ST.Basic (HashTable)
import qualified Data.HashTable.ST.Basic as Table
import Control.Monad.ST (runST, ST)
import Data.STRef (modifySTRef, readSTRef, newSTRef, STRef)

puzzleInput :: [Int]
puzzleInput = [8,0,17,4,1,12]

data GameState = GameState { history :: IntMap Int, currentTurn :: Int } deriving (Show)
data GameState' s = GameState' { history' :: HashTable s Int Int, currentTurn' :: STRef s Int } 

processInput :: [Int] -> GameState
processInput input = GameState { history = Map.fromList $ zip input [0..], currentTurn = length input - 1 }

processInput' :: [Int] -> ST s (GameState' s)
processInput' input = do
  history' <- newFromList $ zip input [0..]
  currentTurn' <- newSTRef $ length input - 1
  pure $ GameState' { history', currentTurn' }
  where
    newFromList xs = do
      hs <- Table.new
      for_ xs (uncurry $ Table.insert hs) 
      pure hs

iter :: GameState -> Int -> (GameState, Int) 
iter GameState{ history, currentTurn } lastRound = (GameState { history = newHistory, currentTurn = currentTurn + 1 }, roundValue)
  where 
    newHistory = Map.insert lastRound currentTurn history
    roundValue = ((currentTurn -) <$> Map.lookup lastRound history) & fromMaybe 0

iter' :: GameState' s -> Int -> ST s Int 
iter' GameState'{ history', currentTurn' } lastRound = do 
  currentTurnVal <- readSTRef currentTurn'
  lastTurnUsed <- Table.lookup history' lastRound
  Table.insert history' lastRound currentTurnVal
  let roundValue = ((currentTurnVal -) <$> lastTurnUsed) & fromMaybe 0
  _ <- modifySTRef currentTurn' (+1)
  pure roundValue

ithNumber :: Int -> [Int] -> Int
ithNumber i input = go (i - length input) (processInput input, viaNonEmpty last input & fromMaybe 0)
  where
    go !turnsLeft (!st, !lastRound)
      | turnsLeft <= 0 = lastRound
      | otherwise = go (turnsLeft - 1) (iter st lastRound)

ithNumber' :: Int -> [Int] -> ST s Int
ithNumber' i unprocessedInput = do
  input <- processInput' unprocessedInput
  go input (i - length unprocessedInput) (viaNonEmpty last unprocessedInput & fromMaybe 0)
  where
    go input turnsLeft lastRound
      | turnsLeft <= 0 = pure lastRound
      | otherwise = go input (turnsLeft - 1) =<< iter' input lastRound

ans :: Int
ans = runST $ ithNumber' 30000000 puzzleInput
