module Day9 where 

import Relude 

fileContents :: IO [Int]
fileContents = do
  contents <- readFileText "./puzzle9.txt"
  pure $ mapMaybe (rightToMaybe . readEither) $ lines contents

windowed :: Int -> [a] -> [[a]]
windowed _ [] = []
windowed n xs@(_:rest) =
  case takeExactly n xs of
    Just window -> window : windowed n rest
    Nothing -> []

  where 
    takeExactly i _ | i <= 0 = Just []
    takeExactly i (y:ys) = (y :) <$> takeExactly (i-1) ys
    takeExactly _ [] = Nothing


unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing 
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = 
  case unsnoc xs of 
    Just (ys, y) -> Just (x:ys, y)
    Nothing -> Nothing 

isValid :: [Int] -> Bool
isValid xs = case unsnoc xs of 
  Nothing -> False
  Just (ys, check) -> not $ null [ a+b | a <- ys, b <- ys, a + b == check ]

firstInvalidNumber :: [Int] -> Maybe Int
firstInvalidNumber inputs = do 
  firstInvalidWindow <- find (not . isValid) $ windowed 26 inputs
  (_, x) <- unsnoc firstInvalidWindow
  pure x

