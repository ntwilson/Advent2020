module Day4 where 

import Relude
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec
import qualified Data.Text as Text

-- all strings because we don't need to bother parsing some of the interesting data formats
data Passport = Passport 
  { birthYear :: String, issueYear :: String, expirationYear :: String, height :: String
  , hairColor :: String, eyeColor :: String, passportID :: String, countryID :: Maybe String
  }
  deriving (Eq, Show)

data PassportField 
  = BirthYear String | IssueYear String | ExpirationYear String | Height String | HairColor String 
  | EyeColor String | PassportID String | CountryID String  
    deriving (Eq, Ord, Show)

type Parser = Parsec.Parsec Void Text 

parsePassportEntries :: Parser [Text]
parsePassportEntries = Parsec.many atMostOneEol
  where
    atMostOneEol = Text.pack <$> Parsec.someTill Parsec.anySingle (atLeastTwoEol <|> onlyWhitespaceLeft)
    atLeastTwoEol = Parsec.try (Parsec.eol *> Parsec.eol $> ())
    onlyWhitespaceLeft = Parsec.try (Parsec.space *> Parsec.eof)

parseAllPassports :: Parser Passport -> [Text] -> [Passport]
parseAllPassports parsePassport = mapMaybe (Parsec.parseMaybe parsePassport)

contents :: IO Text
contents = readFileText "./puzzle4.txt"

passports :: Parser Passport -> IO [Passport]
passports parsePassport = do
  fileContents <- contents
  case Parsec.runParser parsePassportEntries "puzzle4.txt" fileContents of
    Left err -> error $ toText $ Parsec.errorBundlePretty err
    Right passportEntries -> pure $ parseAllPassports parsePassport passportEntries
