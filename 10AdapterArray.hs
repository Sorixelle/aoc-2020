import Debug.Trace
import Data.List
import System.Environment

findDifferences :: Int -> [Int] -> Int
findDifferences diff adapters = loop 0 $ sort (0:adapters)
  where
    loop num (x:y:xs)
      | x + diff == y  = loop (num + 1) (y:xs)
      | otherwise      = loop num (y:xs)
    loop num (x:[])
      | diff == 3 = num + 1
      | otherwise = num

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let joltages = read <$> lines contents
  let part1Sol = (findDifferences 1 joltages) * (findDifferences 3 joltages)
  putStrLn $ "Number of 1 jolt differences multiplied by number of 3 jolt differences (Part 1): " ++ (show part1Sol)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
