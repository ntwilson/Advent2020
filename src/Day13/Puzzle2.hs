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

solution :: [Maybe Int] -> Maybe Int 
solution busIDs = find isValid proposedSolutions
  where
    isValid time = foldl' (&&) True $ imap (isThisBusValid time) busIDs
    isThisBusValid time index (Just busID) = ((index + time) `mod` busID) == 0
    isThisBusValid _ _ Nothing = True

    proposedSolutions = 
      case maxBy snd busesByIndex of 
        Nothing -> []
        Just (i, busID) -> 
          [ x - i | x <- (busID *) <$> [1 .. 1_000_000_000] ] 

    busesByIndex = 
      mapMaybe (\case { (_i, Nothing) -> Nothing; (i, Just x) -> Just (i, x) }) $ itoList busIDs 

    maxBy fn = go Nothing
      where
        go Nothing (x:xs) = go (Just (fn x, x)) xs
        go (Just (y, a)) (x:xs) 
          | fn x > y = go (Just (fn x, x)) xs
          | otherwise = go (Just (y, a)) xs
        go a [] = snd <$> a

ans :: IO Int
ans = do 
  busIDs <- fileContents
  liftEitherMessage $ maybeToRight errMsg $ solution busIDs 
  where
    errMsg :: Text
    errMsg = "Could not find a solution." 
