module Day3.Puzzle2 where

import Relude
import Day3
import qualified Day3.Puzzle1 as Puzzle1

ans :: IO Int
ans = calc <$> contents

slopes :: [Double]
slopes = [1.0, 3.0, 5.0, 7.0, 0.5]

calc :: [MapRow] -> Int
calc mapLayout = p 
  where 
    (Product p) = fold (Product . Puzzle1.calc mapLayout <$> slopes)
