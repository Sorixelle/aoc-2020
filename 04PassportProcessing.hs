import Data.Either
import Data.List
import System.Environment
import Text.Parsec

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

isPassportValid :: [(String, String)] -> Bool
isPassportValid xs =
  all (\f -> elem f fields) required
  where
    fields = fst <$> xs
    required = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

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

main :: IO ()
main = do
  args <- getArgs
  run $ head args
