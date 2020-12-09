import Debug.Trace
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

findWeakness :: Int -> [Int] -> Int
findWeakness invalid xs = loop xs
  where
    loop :: [Int] -> Int
    loop ys
      | ctSum == invalid = (foldl1 max ys) + (foldl1 min ys)
      | length ys == 1   = findWeakness invalid $ tail xs
      | otherwise        = loop $ take (length ys - 1) ys
      where
        ctSum = sum ys

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let ciphertext = read <$> lines contents
  let invalid = findInvalid ciphertext
  putStrLn $ "First invalid number (Part 1): " ++ (show invalid)
  putStrLn $ "Encryption weakness (Part 2): " ++ (show $ findWeakness invalid ciphertext)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
