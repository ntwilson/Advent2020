module Day5.Puzzle1 where 

import Relude 
import Operators
import Day5
import Relude.Extra.Foldable1 (Foldable1(maximum1))

ans :: IO Int
ans = fromMaybe (-1) . viaNonEmpty maximum1 <$> (idOfSeat <$$> rows)

