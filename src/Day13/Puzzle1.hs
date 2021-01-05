module Day13.Puzzle1 where

import Relude
import qualified Prelude
import qualified Control.Monad.Except as Except
import Data.Text (split)
import Relude.Extra.Foldable1

liftEitherMessage :: Show err => Either err a -> IO a
liftEitherMessage (Left msg) = Except.throwError $ Prelude.userError $ show msg
liftEitherMessage (Right a) = pure a

parseInput :: Text -> Either Text (Int, [Int])
parseInput txt = 
  case lines txt of 
    [ts, ids] -> do
      timestamp <- readEither ts
      busIDs <- traverse parseBusID $ split (== ',') ids
      pure (timestamp, catMaybes busIDs)
    xs -> Left ("wrong number of lines in input file for day 13.  Expecting 2, got " <> show (length xs))

  where 
    parseBusID str = 
      case str of 
        "x" -> Right Nothing
        num -> Just <$> readEither num

fileContents :: IO (Int, [Int])
fileContents = liftEitherMessage . parseInput =<< readFileText "./puzzle13.txt"

solution :: Int -> [Int] -> Maybe Int 
solution timestamp busIDs = do
  (departure, busID) <- nextDepartureAndID
  pure $ (departure - timestamp) * busID

  where 
    busSchedules = busIDs <&> \busID -> (busID *) <$> [1..1_000_000]
    nextDepartureForEachBus = fromMaybe 1_000_000_000 . find (> timestamp) <$> busSchedules
    nextDepartureAndID = viaNonEmpty minimum1 $ zip nextDepartureForEachBus busIDs

ans :: IO Int
ans = do 
  (timestamp, busIDs) <- fileContents
  liftEitherMessage $ maybeToRight errMsg $ solution timestamp busIDs 
  where
    errMsg :: Text
    errMsg = "Could not find a departure time that's after the timestamp." 
