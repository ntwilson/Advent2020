module Day3.Puzzle1 where 

import Relude
import Day3

ans :: IO Int
ans = calc <$> contents <*> pure slope
  where slope = 3.0

calc :: [MapRow] -> Double -> Int
calc mapLayout slope = 
  length $ filter (== Just Tree) $ path 0 mapLayout []
  where
    path index (thisRow:rest) acc 
      | (ceiling index :: Int) == floor index = path (index + slope) rest $ (thisRow !!? floor index) : acc
      | otherwise                             = path (index + slope) rest acc 
    path _ [] acc = acc

