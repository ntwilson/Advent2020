module Day13.Puzzle2 where

import Relude
import qualified Prelude
import qualified Control.Monad.Except as Except

import Data.Text (split)
import Control.Lens (itoList, imap)

liftEitherMessage :: Show err => Either err a -> IO a
liftEitherMessage (Left msg) = Except.throwError $ Prelude.userError $ show msg
liftEitherMessage (Right a) = pure a

parseInput :: Text -> Either Text [Maybe Int]
parseInput txt = 
  case lines txt of 
    [_ts, ids] -> traverse parseBusID $ split (== ',') ids
    xs -> Left ("wrong number of lines in input file for day 13.  Expecting 2, got " <> show (length xs))

  where 
    parseBusID str = 
      case str of 
        "x" -> Right Nothing
        num -> Just <$> readEither num

fileContents :: IO [Maybe Int]
fileContents = liftEitherMessage . parseInput =<< readFileText "./puzzle13.txt"

solutionSlow :: [Maybe Int] -> Maybe Int 
solutionSlow busIDs = find isValid proposedSolutions
  where
    busesToCheck = sortOn negate $ catMaybes busIDs
    isValid time = all id $ imap (isThisBusValid time) busesToCheck
    isThisBusValid time index busID = ((index + time) `mod` busID) == 0

    proposedSolutions = 
      case maxBy snd busesByIndex of 
        Nothing -> []
        Just (i, busID) -> 
          [ x - i | x <- (busID *) <$> [10_000_000_000 .. 100_000_000_000] ] 

    busesByIndex = 
      mapMaybe (\case { (_i, Nothing) -> Nothing; (i, Just x) -> Just (i, x) }) $ itoList busIDs 

    maxBy fn = go Nothing
      where
        go Nothing (x:xs) = go (Just (fn x, x)) xs
        go (Just (y, a)) (x:xs) 
          | fn x > y = go (Just (fn x, x)) xs
          | otherwise = go (Just (y, a)) xs
        go a [] = snd <$> a

-- Got stumped.  Had to copy the strategy from https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md#day-13
solution :: [Maybe Int] -> Maybe Int 
solution busIDs =
  fst $
    foldl' 
      (\(minSolution, step) (nextIndex, nextBusID) -> 
        let 
          newMin = case minSolution of 
            Nothing -> Nothing 
            Just s -> [s, s + step .. s + (100_000 * step)] & find (\a -> (a + nextIndex) `mod` nextBusID == 0)
          newStep = step * nextBusID
        in (newMin, newStep))
      (Just 1, 1)
      (sortBy (comparing (negate . snd)) busesByIndex)

  where 
    busesByIndex = 
      mapMaybe (\case { (_i, Nothing) -> Nothing; (i, Just x) -> Just (i, x) }) $ itoList busIDs 

ans :: IO Int
ans = do 
  busIDs <- fileContents
  liftEitherMessage $ maybeToRight errMsg $ solution busIDs 
  where
    errMsg :: Text
    errMsg = "Could not find a solution." 
