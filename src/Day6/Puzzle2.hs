module Day6.Puzzle2 where

import Relude
import Day6
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Text.Megaparsec as Parsec


sharedChars :: Text -> [Char]
sharedChars allResponses = 
  let nResponses = length $ lines allResponses
  in mapMaybe (viaNonEmpty head) $ filter ((== nResponses) . length) $ group $ sort $ filter (not . isSpace) $ Text.unpack allResponses

ans :: IO Int
ans = do
  fileContents <- contents
  case Parsec.runParser parseFlightGroups "puzzle6.txt" fileContents of
    Left err -> error $ toText $ Parsec.errorBundlePretty err
    Right groups -> pure $ sum $ length . sharedChars <$> groups
