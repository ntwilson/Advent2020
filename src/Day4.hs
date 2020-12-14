{-# LANGUAGE LambdaCase #-}
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

fieldsToPassport :: [PassportField] -> Maybe Passport
fieldsToPassport passportVals = do
  birthYear <- viaNonEmpty head $ mapMaybe (\case { BirthYear yr -> Just yr; _ -> Nothing }) passportVals
  issueYear <- viaNonEmpty head $ mapMaybe (\case { IssueYear x -> Just x; _ -> Nothing }) passportVals
  expirationYear <- viaNonEmpty head $ mapMaybe (\case { ExpirationYear x -> Just x; _ -> Nothing }) passportVals
  height <- viaNonEmpty head $ mapMaybe (\case { Height x -> Just x; _ -> Nothing }) passportVals
  hairColor <- viaNonEmpty head $ mapMaybe (\case { HairColor x -> Just x; _ -> Nothing }) passportVals
  eyeColor <- viaNonEmpty head $ mapMaybe (\case { EyeColor x -> Just x; _ -> Nothing }) passportVals
  passportID <- viaNonEmpty head $ mapMaybe (\case { PassportID x -> Just x; _ -> Nothing }) passportVals
  let countryID = viaNonEmpty head $ mapMaybe (\case { CountryID x -> Just x; _ -> Nothing }) passportVals
  pure $ Passport { birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportID, countryID }

parsePassport :: Parser PassportField -> Parser Passport
parsePassport parseKeyValue = do
  keysNVals <- Parsec.sepBy parseKeyValue Parsec.spaceChar
  case fieldsToPassport keysNVals of 
    Just ppt -> pure ppt
    Nothing -> fail "Could not build a passport out of the given keys and values"

parseAllPassports :: Parser PassportField -> [Text] -> [Passport]
parseAllPassports parseKeyValue = mapMaybe (Parsec.parseMaybe (parsePassport parseKeyValue))

contents :: IO Text
contents = readFileText "./puzzle4.txt"

passports :: Parser PassportField -> IO [Passport]
passports parseKeyValue = do
  fileContents <- contents
  case Parsec.runParser parsePassportEntries "puzzle4.txt" fileContents of
    Left err -> error $ toText $ Parsec.errorBundlePretty err
    Right passportEntries -> pure $ parseAllPassports parseKeyValue passportEntries
