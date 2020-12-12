module Day4 where 

import Relude
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec
import qualified Data.Map.Strict as Map

-- all strings because we don't need to bother parsing some of the interesting data formats
data Passport = Passport 
  { birthYear :: String, issueYear :: String, expirationYear :: String, height :: String
  , hairColor :: String, eyeColor :: String, passportID :: String, countryID :: Maybe String
  }
  deriving (Eq, Show)

data PassportField 
  = BirthYear | IssueYear | ExpirationYear | Height | HairColor | EyeColor | PassportID | CountryID  
    deriving (Eq, Ord, Show)

type Parser = Parsec.Parsec Void Text 

data ParsedPassport = InvalidPassport | ValidPassport Passport deriving (Eq, Show) 

parseKeyValue :: Parser (PassportField, String)
parseKeyValue = Parsec.try $ do 
  key <- Parsec.choice 
    [ Parsec.string "byr" $> BirthYear
    , Parsec.string "iyr" $> IssueYear
    , Parsec.string "eyr" $> ExpirationYear
    , Parsec.string "hgt" $> Height
    , Parsec.string "hcl" $> HairColor
    , Parsec.string "ecl" $> EyeColor
    , Parsec.string "pid" $> PassportID
    , Parsec.string "cid" $> CountryID
    ]
  _ <- Parsec.char ':' 
  val <- Parsec.some (Parsec.alphaNumChar <|> Parsec.symbolChar <|> Parsec.punctuationChar)
  pure (key, val)

fieldsToPassport :: [(PassportField, String)] -> Maybe Passport
fieldsToPassport fields = do
  birthYear <- Map.lookup BirthYear passportVals
  issueYear <- Map.lookup IssueYear passportVals
  expirationYear <- Map.lookup ExpirationYear passportVals
  height <- Map.lookup Height passportVals
  hairColor <- Map.lookup HairColor passportVals
  eyeColor <- Map.lookup EyeColor passportVals
  passportID <- Map.lookup PassportID passportVals
  pure $ Passport { birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportID, countryID = Map.lookup CountryID passportVals }
  where
    passportVals = Map.fromList fields

parsePassport :: Parser ParsedPassport
parsePassport = do
  -- consumes the first newline at the end of a passport entry
  keysNVals <- Parsec.sepEndBy parseKeyValue Parsec.spaceChar
  case fieldsToPassport keysNVals of 
    Just ppt -> pure $ ValidPassport ppt
    Nothing -> pure InvalidPassport

parseAllPassports :: Parser [ParsedPassport]
parseAllPassports = Parsec.sepEndBy parsePassport Parsec.eol

contents :: IO Text
contents = readFileText "./puzzle4.txt"

passports :: IO [ParsedPassport]
passports = do
  file <- contents
  case Parsec.runParser parseAllPassports "Puzzle4.txt" file of
    Left err -> do
      putTextLn "Parsing failed"
      putStrLn $ Parsec.errorBundlePretty err
      error $ toText $ Parsec.errorBundlePretty err
    Right psprts -> pure psprts

