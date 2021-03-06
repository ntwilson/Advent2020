module Day1 where 

import Relude

expenseReport :: IO [Int]
expenseReport = do
  contents <- readFileText "./puzzle1.txt"
  pure $ catMaybes (lines contents <&> (readMaybe . toString))