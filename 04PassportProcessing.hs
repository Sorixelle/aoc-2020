import Data.Either
import Data.List
import System.Environment
import Text.Parsec
import Text.Read

fieldName :: Parsec String () String
fieldName = do
  name <- foldr1 (<|>) $ (try . string) <$> ["byr","iyr","eyr","hgt","hcl","ecl","pid","cid"]
  _ <- char ':'
  return name

field :: Parsec String () (String, String)
field = do
  name <- fieldName
  value <- many1 (try (char '#') <|> alphaNum)
  return (name, value)

passport :: Parsec String () [(String, String)]
passport = sepBy field space

height :: Parsec String () (Int, String)
height = do
  val <- many1 digit
  unit <- string "in" <|> string "cm"
  return (read val, unit)

colour :: Parsec String () String
colour = do
  _ <- char '#'
  hexCode <- count 6 hexDigit
  return $ '#' : hexCode

isPassportValid :: [(String, String)] -> Bool
isPassportValid xs =
  all (\f -> elem f fields) required
  where
    fields = fst <$> xs
    required = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

isFieldValid :: (String, String) -> Bool
isFieldValid ("byr",byr) =
  case readMaybe byr of
    Just y -> y >= 1920 && y <= 2002
    Nothing -> False
isFieldValid ("iyr",iyr) =
  case readMaybe iyr of
    Just y -> y >= 2010 && y <= 2020
    Nothing -> False
isFieldValid ("eyr",eyr) =
  case readMaybe eyr of
    Just y -> y >= 2020 && y <= 2030
    Nothing -> False
isFieldValid ("hgt",hgt)
  | unit == "in" = val >= 59 && val <= 76
  | unit == "cm" = val >= 150 && val <= 193
  | otherwise    = False
  where
    (val, unit) = fromRight (0,"") $ parse height "" hgt
isFieldValid ("hcl",hcl) =
  case parse colour "" hcl of
    Right _ -> True
    Left _ -> False
isFieldValid ("ecl",ecl) = elem ecl ["amb","blu","brn","gry","grn","hzl","oth"]
isFieldValid ("pid",pid) =
  case readMaybe pid :: Maybe Int of
    Just i -> length pid == 9
    Nothing -> False
isFieldValid ("cid",_) = True
isFieldValid _ = False

isPassportValid' :: [(String, String)] -> Bool
isPassportValid' p = all isFieldValid p && isPassportValid p

constructPassports :: [String] -> String -> [String]
constructPassports xs "" = "" : xs
constructPassports ("":xs) s = s : xs
constructPassports (x:xs) s = (x ++ " " ++ s) : xs
constructPassports xs s = [s]

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let entries = foldl' constructPassports [] $ lines contents
  let passports = (fromRight [("","")]) . (parse passport "") <$> entries
  let numValidPassports = length $ filter id $ isPassportValid <$> passports
  putStrLn $ "Number of valid passports (Part 1): " ++ (show numValidPassports)
  let numValidPassportsStrict = length $ filter id $ isPassportValid' <$> passports
  putStrLn $ "Number of passports with valid data (Part 2): " ++ (show numValidPassportsStrict)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
