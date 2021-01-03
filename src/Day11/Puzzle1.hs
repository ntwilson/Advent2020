module Day11.Puzzle1 where

import Relude
import Data.Array (Array)
import Operators
import qualified Data.Array as Array 
import Control.Lens (imap)
import AoCArray

fileContents :: IO Text
fileContents = readFileText "./puzzle11.txt"

data Seat = Empty Int Int | Occupied Int Int | NotAvailable Int Int deriving (Show, Eq)
type FloorPlan = Array Int (Array Int Seat)

isOccupied :: Seat -> Bool
isOccupied (Occupied _ _) = True
isOccupied _ = False


doubleIndex :: Array.Ix i => Array i (Array i e) -> i -> i -> Maybe e
doubleIndex ar i1 i2 = (ar ! i1) >>= (! i2)

adjacent :: FloorPlan -> Int -> Int -> [Seat]
adjacent plan row column = 
  [ fromMaybe (NotAvailable r c) $ doubleIndex plan r c
  | r <- (row +) <$> [(-1) .. 1], c <- (column +) <$> [(-1) .. 1]
  , (r, c) /= (row, column)
  ]

advanceSeat :: FloorPlan -> Seat -> Seat
advanceSeat _ (NotAvailable r c) = NotAvailable r c
advanceSeat plan (Empty r c) = 
  if not . any isOccupied $ adjacent plan r c 
  then Occupied r c else Empty r c
  
advanceSeat plan (Occupied r c) = 
  if length (filter isOccupied $ adjacent plan r c) >= 4
  then Empty r c else Occupied r c

parseFloorPlan :: Text -> Maybe FloorPlan
parseFloorPlan txt = sequence $ arrayFromListStartingAt 0 $ imap parseLine $ lines txt 
  
  where 
    parseLine row ln =  sequence $ arrayFromListStartingAt 0 $ imap (parseSeat row) $ toString ln

    parseSeat row col '.' = Just $ NotAvailable row col
    parseSeat row col 'L' = Just $ Empty row col
    parseSeat row col '#' = Just $ Occupied row col
    parseSeat _ _ _ = Nothing

fileFloorPlan :: IO FloorPlan 
fileFloorPlan = do
  contents <- fileContents
  case parseFloorPlan contents of
    Nothing -> error "unable to parse puzzle input"
    Just plan -> pure plan

advanceFloorPlan :: FloorPlan -> FloorPlan 
advanceFloorPlan plan = advanceSeat plan <$$> plan

solution :: FloorPlan -> Maybe Int 
solution plan = do
  s <- stabilized 
  pure $ sum (length . filter isOccupied . arrayToList <$> s) 
  where
    stabilized = find (uncurry (==)) successivePlans <&> fst
    successivePlans = pairwise $ scanl (\p _ -> advanceFloorPlan p) plan [(1::Int) .. 1000]
    pairwise (a:b:rest) = (a,b) : pairwise rest
    pairwise _ = []


ans :: IO Int
ans = do
  s <- solution <$> fileFloorPlan
  case s of 
    Nothing -> error "Could not find a floor plan that eventually stabilized"
    Just a -> pure a
