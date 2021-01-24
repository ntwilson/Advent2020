module Day15.Puzzle2 where

import Relude 
import Control.Monad.ST (runST, ST)
import Data.STRef (writeSTRef, modifySTRef, readSTRef, newSTRef, STRef)
import qualified Data.Array.MArray as Array
import Data.Array.Base (newArray, STUArray)
import Data.List (maximum)

puzzleInput :: [Int]
puzzleInput = [8,0,17,4,1,12]

data GameState s = GameState { history :: STUArray s Int Int, currentTurn :: STRef s Int, lastRound :: STRef s Int } 

processInput :: Int -> [Int] -> ST s (GameState s)
processInput size input = do
  history <- newFromList $ zip input [0..]
  currentTurn <- newSTRef $ length input - 1
  lastRound <- newSTRef (viaNonEmpty last input & fromMaybe 0)
  pure $ GameState { history, currentTurn, lastRound }
  where
    newFromList :: [(Int, Int)] -> ST s (STUArray s Int Int)
    newFromList xs = do
      let computedSize = max (size - 1) (maximum input)
      ar <- newArray (0, computedSize) (-1)
      for_ xs (uncurry $ Array.writeArray ar)
      pure ar

iter :: GameState s -> ST s () 
iter GameState{ history, currentTurn, lastRound } = do 
  currentTurnVal <- readSTRef currentTurn
  lastRoundVal <- readSTRef lastRound
  lastTurnUsed <- tryReadArray history lastRoundVal
  Array.writeArray history lastRoundVal currentTurnVal
  let roundValue = ((currentTurnVal -) <$> lastTurnUsed) & fromMaybe 0
  _ <- modifySTRef currentTurn (+1)
  _ <- writeSTRef lastRound roundValue
  pure ()
  where
    tryReadArray ar i = do
      bounds <- Array.getBounds ar
      if Array.inRange bounds i
      then do
        x <- Array.readArray ar i
        if x == -1 then pure Nothing else pure (Just x)
      else pure Nothing

ithNumber :: Int -> [Int] -> Int
ithNumber i unprocessedInput = runST $ do
  input <- processInput i unprocessedInput
  for_ [1 .. i - length unprocessedInput] $ const $ iter input 
  readSTRef $ lastRound input

ans :: Int
ans = ithNumber 30000000 puzzleInput
