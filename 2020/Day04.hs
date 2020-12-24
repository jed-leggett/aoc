{-# LANGUAGE OverloadedStrings #-}
module Day03 where

import Control.Monad(void)
import Data.Either (rights, lefts)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Text (unpack)
-- import Control.Applicative((<|>))

import AOC

data ShouldValidateExtra =
    Yes
  | No

mainA :: IO ()
mainA = do
  ls <- readFileText "Day04Input"
  let passports = (parseString (manyTill (parsePassport No) (try eof))) (unpack ls)
  mapM_ print $ lefts passports
  print . length $ rights passports


mainB :: IO ()
mainB = do
  ls <- readFileText "Day04Input"
  let passports = (parseString (manyTill (parsePassport Yes) (try eof))) (unpack ls)
  mapM_ print $ lefts passports
  print . length $ rights passports

newtype BirthYear = BirthYear String deriving Show
newtype IssueYear = IssueYear String deriving Show
newtype ExpirationYear = ExpirationYear String deriving Show
newtype Height = Height String deriving Show
newtype HairColor = HairColor String deriving Show
newtype EyeColor = EyeColor String deriving Show
newtype PassportId = PassportId String deriving Show
newtype CountryId = CountryId String deriving Show

data Passport =
  Passport
  { birthYear      :: BirthYear
  , countryId      :: Maybe CountryId
  , eyeColor       :: EyeColor
  , expirationYear :: ExpirationYear
  , hairColor      :: HairColor
  , height         :: Height
  , issueYear      :: IssueYear
  , passportId     :: PassportId
  } deriving Show

parsePassport :: ShouldValidateExtra -> Parser (Either String Passport)
parsePassport sv = do
  passportBlock <- getPassportBlock
  let meh = sort passportBlock
  pure $ buildPassport sv meh


getPassportBlock :: Parser [String]
getPassportBlock = do
  lines <- fmap (splitOn " ") (manyTill anyChar (void (char '\n') <|> eof))
  next <- optionMaybe (void (char '\n') <|> eof)
  case next of
    Just _  -> pure lines
    Nothing -> fmap (++lines) getPassportBlock


buildPassport :: ShouldValidateExtra -> [String] -> Either String Passport
buildPassport b (s1:s2:s3:s4:s5:s6:s7:s8:[]) =
  Passport
    <$> (parseStringOrError (pareseBirthYear b) s1)
    <*> (fmap Just $ parseStringOrError (pareseCountryId b) s2)
    <*> (parseStringOrError (pareseEyeColor b) s3)
    <*> (parseStringOrError (pareseExpirationYear b) s4)
    <*> (parseStringOrError (pareseHairColor b) s5)
    <*> (parseStringOrError (pareseHeight b) s6)
    <*> (parseStringOrError (pareseIssueYear b) s7)
    <*> (parseStringOrError (paresePassportId b) s8)
buildPassport b (s1:s2:s3:s4:s5:s6:s7:[]) =
  Passport
    <$> (parseStringOrError (pareseBirthYear b) s1)
    <*> (pure Nothing)
    <*> (parseStringOrError (pareseEyeColor b) s2)
    <*> (parseStringOrError (pareseExpirationYear b) s3)
    <*> (parseStringOrError (pareseHairColor b) s4)
    <*> (parseStringOrError (pareseHeight b) s5)
    <*> (parseStringOrError (pareseIssueYear b) s6)
    <*> (parseStringOrError (paresePassportId b) s7)
buildPassport _ foo =
  Left (show foo)

pareseBirthYear :: ShouldValidateExtra -> Parser BirthYear
pareseBirthYear ev = do
  string "byr:"
  BirthYear <$>
    case ev of
      Yes -> do
        year <- many1 digit
        fmap show $ validateYear (read year) 1920 2002
      No -> many1 digit

pareseIssueYear :: ShouldValidateExtra -> Parser IssueYear
pareseIssueYear ev = do
  string "iyr:"
  IssueYear <$>
    case ev of
      Yes -> do
        year <- many1 digit
        fmap show $ validateYear (read year) 2010 2020
      No -> many1 digit

pareseExpirationYear :: ShouldValidateExtra -> Parser ExpirationYear
pareseExpirationYear ev = do
  string "eyr:"
  ExpirationYear <$>
    case ev of
      Yes -> do
        year <- many1 digit
        fmap show $ validateYear (read year) 2020 2030
      No -> many1 digit

pareseHeight :: ShouldValidateExtra -> Parser Height
pareseHeight ev = do
  string "hgt:"
  Height <$>
    case ev of
      Yes -> do
        num <- many1 digit
        let numInt = read num
        unt <- many1 letter
        case unt of
          "cm" -> if (numInt :: Int) < 150 || numInt > 193
            then fail $ "invalid height: " ++ num ++ unt
            else pure $ num ++ unt
          "in" -> if numInt < 59 || numInt > 76
            then fail $ "invalid height: " ++ num ++ unt
            else pure $ num ++ unt
          _    -> fail $ "invalid height: " ++ num ++ unt
      No -> many1 anyChar

pareseHairColor :: ShouldValidateExtra -> Parser HairColor
pareseHairColor ev = do
  string "hcl:"
  HairColor <$>
    case ev of
      Yes -> do
        char '#'
        clr <- count 6 (choice (char <$> ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']))
        try (void (char '\n') <|> eof)
        pure $ "#" ++ clr
      No -> many1 anyChar

pareseEyeColor :: ShouldValidateExtra -> Parser EyeColor
pareseEyeColor ev = do
  string "ecl:"
  EyeColor <$>
    case ev of
      Yes -> do
        clr <- choice (try . string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
        -- try (void (char '\n') <|> eof)
        pure clr
      No -> many1 anyChar

paresePassportId :: ShouldValidateExtra -> Parser PassportId
paresePassportId ev = do
  string "pid:"
  PassportId <$>
    case ev of
      Yes -> do
        passId <- count 9 digit
        try eof
        pure passId
      No -> many1 anyChar

pareseCountryId :: ShouldValidateExtra -> Parser CountryId
pareseCountryId ev = do
  string "cid:"
  CountryId <$>
    case ev of
      Yes -> many1 anyChar
      No -> many1 anyChar

validateYear :: Int -> Int -> Int -> Parser Int
validateYear year low high=
  if year < low || year > high
    then fail ("year invalid: " ++ show year)
    else pure year
