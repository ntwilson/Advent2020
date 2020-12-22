module Day6 where

import Relude
import Text.Megaparsec
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char
import qualified Data.Text as Text
import Data.Char (isSpace)

contents :: IO Text
contents = readFileText "./puzzle6.txt"

type Parser = Parsec Void Text

parseFlightGroups :: Parser [Text]
parseFlightGroups = Parsec.many atMostOneEol
  where
    atMostOneEol = Text.pack <$> someTill anySingle (atLeastTwoEol <|> onlyWhitespaceLeft)
    atLeastTwoEol = try (eol *> eol $> ())
    onlyWhitespaceLeft = try (space *> eof)

uniqueChars :: Text -> [Char]
uniqueChars = filter (not . isSpace) . mapMaybe (viaNonEmpty head) . group . sort . Text.unpack

ans :: IO Int
ans = do
  fileContents <- contents
  case Parsec.runParser parseFlightGroups "puzzle6.txt" fileContents of
    Left err -> error $ toText $ Parsec.errorBundlePretty err
    Right groups -> pure $ sum $ length . uniqueChars <$> groups
