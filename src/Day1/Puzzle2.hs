module Day1.Puzzle2 where

import Relude
import Day1 (expenseReport)
import Operators

ans :: IO Int
ans = calc <$> expenseReport

calc :: [Int] -> Int
calc xs = x * y * z
  where (x, y, z) = viaNonEmpty head [(x', y', z') | x' <- xs, y' <- xs, z' <- xs, x' + y' + z' == 2020] # fromMaybe (0,0,0)
