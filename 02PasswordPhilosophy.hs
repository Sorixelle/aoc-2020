import Text.ParserCombinators.ReadP
import System.Environment

data Policy = Policy
  { character :: Char
  , num1 :: Int
  , num2 :: Int
  , password :: String
  }

digit :: ReadP Char
digit = satisfy (\c -> c >= '0' && c <= '9')

number :: ReadP Int
number = fmap read (many1 digit)

letter :: ReadP Char
letter = satisfy (\c -> c >= 'a' && c <= 'z')

range :: ReadP (Int, Int)
range = do
  min <- number
  char '-'
  max <- number
  return (min, max)

policyParser :: ReadP Policy
policyParser = do
  (min, max) <- range
  char ' '
  c <- letter
  string ": "
  pwd <- many1 letter
  eof
  return (Policy c min max pwd)

parsePolicy :: String -> Policy
parsePolicy s = (fst . head) $ readP_to_S policyParser s

validatePolicy :: Policy -> Bool
validatePolicy (Policy c min max pwd) =
  n >= min && n <= max
  where
    n = length $ filter (c ==) pwd

validatePolicy' :: Policy -> Bool
validatePolicy' (Policy c pos1 pos2 pwd) =
  (char1 == c) /= (char2 == c)
  where
    char1 = pwd !! (pos1 - 1)
    char2 = pwd !! (pos2 - 1)

numValidPolicies :: (Policy -> Bool) -> [Policy] -> Int
numValidPolicies f ps = length $ filter f ps

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let policies = parsePolicy <$> lines contents
  putStrLn $ "Number of valid policies (Part 1): " ++ show (numValidPolicies validatePolicy policies)
  putStrLn $ "Number of valid policies (Part 2): " ++ show (numValidPolicies validatePolicy' policies)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
