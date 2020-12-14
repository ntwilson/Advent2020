module Day5.Puzzle2 where

import Relude
import Operators
import Day5

findMySeat :: [Int] -> Maybe Int 
findMySeat takenSeats = 
  firstJust seatMissing $ windowed 3 $ sort takenSeats
  where
    windowed windowSize xs@(_:rest) = 
      let firstN = take windowSize xs
      in 
        if length firstN == windowSize
        then firstN : windowed windowSize rest
        else []
    windowed _ [] = []

    seatMissing [a,b,c] 
      | a+2 == b && b+1 == c  = Just (a+1)
      | a+1 == b && b+2 == c  = Just (b+1)
      | otherwise             = Nothing
    seatMissing _             = Nothing

    firstJust f = listToMaybe . mapMaybe f

ans :: IO Int
ans = fromMaybe 0 . findMySeat <$> (idOfSeat <$$> rows)