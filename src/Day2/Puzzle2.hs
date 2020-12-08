module Day2.Puzzle2 where

import Relude
import Day2

ans :: IO Int
ans = calc <$> passwords

-- | Exclusive Or
(|||) :: Bool -> Bool -> Bool
True  ||| True  = False
True  ||| False = True
False ||| True  = True
False ||| False = False

calc :: [PasswordLine] -> Int
calc = length . filter isValid 
  where 
    isValid line = 
      let 
        reqs = policy line
        expected = requiredChar reqs
        pwd = toString $ password line
        firstChar = pwd !!? (occurrencesMin reqs - 1)
        lastChar  = pwd !!? (occurrencesMax reqs - 1)
      in (firstChar == Just expected) ||| (lastChar == Just expected)
