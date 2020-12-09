module Day1.Puzzle1 where

import Relude
import Day1 (expenseReport)

ans :: IO Int
ans = calc <$> expenseReport 

calc :: [Int] -> Int
calc xs = x * y 
  where (x, y) = viaNonEmpty head [(x', y') | x' <- xs, y' <- xs, x' + y' == 2020] & fromMaybe (0,0)

