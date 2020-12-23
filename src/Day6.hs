module Day6 where

import Relude
import Text.Megaparsec
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char
import qualified Data.Text as Text

contents :: IO Text
contents = readFileText "./puzzle6.txt"

type Parser = Parsec Void Text

parseFlightGroups :: Parser [Text]
parseFlightGroups = Parsec.many atMostOneEol
  where
    atMostOneEol = Text.pack <$> someTill anySingle (atLeastTwoEol <|> onlyWhitespaceLeft)
    atLeastTwoEol = try (eol *> eol $> ())
    onlyWhitespaceLeft = try (space *> eof)
