module Day9.Puzzle2 where

import Relude
import Day9
import Relude.Extra.Foldable1 (Foldable1(minimum1, maximum1))

sumOfEndpoints :: [Int] -> Int -> Maybe Int
sumOfEndpoints [] _ = Nothing
sumOfEndpoints [_] _ = Nothing
sumOfEndpoints inputs@(_:rest) invalidNum = go [] inputs
  where
    go _ [] = Nothing
    go [] (x:xs) = go [x] xs
    go acc (y:ys) 
      | sum (y:acc) == invalidNum = do 
        mn <- viaNonEmpty minimum1 (y:acc)
        mx <- viaNonEmpty maximum1 (y:acc)
        pure $ mn + mx
      | sum (y:acc) < invalidNum = go (y:acc) ys
      | otherwise = sumOfEndpoints rest invalidNum 

solution :: [Int] -> Maybe Int
solution inputs = sumOfEndpoints inputs =<< firstInvalidNumber inputs

ans :: IO Int
ans = do 
  contents <- fileContents
  case solution contents of
    Just a -> pure a
    Nothing -> error "No solution found."
