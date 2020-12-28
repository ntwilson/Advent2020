module Day10.Puzzle1 where

import Relude
import qualified Prelude
import Operators
import qualified Control.Monad.Except as Except
import Relude.Extra.Foldable1 (maximum1)

liftEitherMessage :: Show err => Either err a -> IO a
liftEitherMessage (Left msg) = Except.throwError $ Prelude.userError $ show msg
liftEitherMessage (Right a) = pure a

fileContents :: IO [Int]
fileContents = do
  lineAttempts <- readFileText "./puzzle10.txt" <&> lines <&&> readEither 
  contents <- traverse liftEitherMessage lineAttempts
  case viaNonEmpty maximum1 contents of
    Nothing -> pure [0]
    Just m -> pure (0 : m + 3 : contents)

steps :: [Int] -> [Int]
steps = fmap length . group . sort . fmap (\(a, b) -> b - a) . pairwise . sort

pairwise :: [a] -> [(a, a)]
pairwise (x:y:rest) = (x,y) : pairwise (y:rest)
pairwise _ = []

ans :: IO [Int]
ans = steps <$> fileContents
