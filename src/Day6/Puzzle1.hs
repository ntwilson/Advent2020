module Day6.Puzzle1 where

import Relude
import Day6
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Text.Megaparsec as Parsec

uniqueChars :: Text -> [Char]
uniqueChars = filter (not . isSpace) . mapMaybe (viaNonEmpty head) . group . sort . Text.unpack

ans :: IO Int
ans = do
  fileContents <- contents
  case Parsec.runParser parseFlightGroups "puzzle6.txt" fileContents of
    Left err -> error $ toText $ Parsec.errorBundlePretty err
    Right groups -> pure $ sum $ length . uniqueChars <$> groups
