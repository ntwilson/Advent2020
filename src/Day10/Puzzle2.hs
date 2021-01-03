module Day10.Puzzle2 where

import Relude
import Operators
import AoCArray
import qualified Prelude
import qualified Control.Monad.Except as Except
import Relude.Extra.Foldable1
import qualified Data.Array as Array
import Control.Lens (FunctorWithIndex(imap))

liftEitherMessage :: Show err => Either err a -> IO a
liftEitherMessage (Left msg) = Except.throwError $ Prelude.userError $ show msg
liftEitherMessage (Right a) = pure a

fileContents :: IO [Int64]
fileContents = do
  lineAttempts <- readFileText "./puzzle10.txt" <&> lines <&&> readEither 
  contents <- traverse liftEitherMessage lineAttempts
  case viaNonEmpty maximum1 contents of
    Nothing -> pure [0]
    Just m -> pure (0 : m + 3 : contents)

solution :: [Int64] -> Int64
solution inputs = solutionsStartingAt 0
  where 
    solutionsStartingAt i = fromMaybe 0 (solutions ! i)

    solutions = Array.listArray (0, length inputs - 1) solutionList
      where 
        solutionList = imap calculateSolutionAt $ lastN $ sort inputs

        lastN lst@(_:ys) = lst : lastN ys
        lastN [] = []

    calculateSolutionAt i (w:_x:y:z:_)  
      | z - w <= 3 = solutionsStartingAt (i+1) + solutionsStartingAt (i+2) + solutionsStartingAt (i+3)
      | y - w <= 3 = solutionsStartingAt (i+1) + solutionsStartingAt (i+2)
      | otherwise = solutionsStartingAt (i+1)

    calculateSolutionAt i (x:_y:z:_) 
      | z - x <= 3 = solutionsStartingAt (i+1) + solutionsStartingAt (i+2)
      | otherwise = solutionsStartingAt (i+1)

    calculateSolutionAt _ _ = 1


ans :: IO Int64
ans = solution <$> fileContents
