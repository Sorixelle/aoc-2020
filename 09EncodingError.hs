import Data.List
import System.Environment

sums :: [Int] -> [Int]
sums xs = nub $ sum <$> (filter duplicates $ sequence [xs, xs])
  where
    duplicates (x:y:[]) = x /= y

findInvalid :: [Int] -> Int
findInvalid xs
  | num `elem` validNums = findInvalid $ tail xs
  | otherwise            = num
  where
    (preamble, nums) = splitAt 25 xs
    validNums = sums preamble
    num = head nums

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let ciphertext = read <$> lines contents
  putStrLn $ "First invalid number (Part 1): " ++ (show $ findInvalid ciphertext)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
