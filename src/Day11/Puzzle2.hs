module Day11.Puzzle2 where

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

isSittable :: Seat -> Bool
isSittable (NotAvailable _ _) = False
isSittable _ = True


doubleIndex :: Array.Ix i => Array i (Array i e) -> i -> i -> Maybe e
doubleIndex ar i1 i2 = (ar ! i1) >>= (! i2)

-- Assumes the floor plan arrays are indexed with a base of 0
visible :: FloorPlan -> Int -> Int -> [Seat]
visible plan row col = 
  fromMaybe (NotAvailable row col) . (find isSittable . catMaybes . takeWhile isJust) <$>
    [ [Just plan !? (row - a) !? col       | a <- [1..]] -- ^
    , [Just plan !? (row - a) !? (col + a) | a <- [1..]] -- ^>
    , [Just plan !? row !? (col + a)       | a <- [1..]] -- >
    , [Just plan !? (row + a) !? (col + a) | a <- [1..]] -- v>
    , [Just plan !? (row + a) !? col       | a <- [1..]] -- v
    , [Just plan !? (row + a) !? (col - a) | a <- [1..]] -- <v
    , [Just plan !? row !? (col - a)       | a <- [1..]] -- <
    , [Just plan !? (row - a) !? (col - a) | a <- [1..]] -- <^
    ]

advanceSeat :: FloorPlan -> Seat -> Seat
advanceSeat _ (NotAvailable r c) = NotAvailable r c
advanceSeat plan (Empty r c) = 
  if not . any isOccupied $ visible plan r c 
  then Occupied r c else Empty r c
  
advanceSeat plan (Occupied r c) = 
  if length (filter isOccupied $ visible plan r c) >= 5
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
