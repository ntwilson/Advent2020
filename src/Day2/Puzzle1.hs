module Day2.Puzzle1 where 

import Relude
import Day2

ans :: IO Int
ans = calc <$> passwords

calc :: [PasswordLine] -> Int
calc = length . filter isValid 
  where 
    isValid line = 
      let numExpectedChar = length $ filter (== (requiredChar $ policy line)) $ toString $ password line
      in occurrencesMin (policy line) <= numExpectedChar && numExpectedChar <= occurrencesMax (policy line)
