import Data.List
import Data.Maybe
import System.Environment

data Tree = Leaf | Node Int Tree Tree Tree deriving Show

findDifferences :: Int -> [Int] -> Int
findDifferences diff adapters = loop 0 $ sort (0:adapters)
  where
    loop num (x:y:xs)
      | x + diff == y  = loop (num + 1) (y:xs)
      | otherwise      = loop num (y:xs)
    loop num (x:[])
      | diff == 3 = num + 1
      | otherwise = num

constructTree :: [Int] -> Tree
constructTree xs = loop 0
  where
    xs' = 0:(xs ++ [3 + foldl1' max xs])
    loop x
      | x `elem` xs' = Node x (loop $ x+1) (loop $ x+2) (loop $ x+3)
      | otherwise    = Leaf

findNumCombinations :: [Int] -> Int
findNumCombinations joltages = fromJust $ lookup 0 $ (snd $ traverse [] 0 $ constructTree joltages)
  where
    maxJoltage = 3 + foldl1' max joltages
    traverse visited acc (Node value l m r)
      | known >= 0          = (known, visited)
      | value == maxJoltage = (acc + 1, visited)
      | otherwise           = (total, newVisited)
      where
        known = fromMaybe (-1) $ lookup value visited
        (left, lVisited) = traverse visited 0 l
        (middle, mVisited) = traverse lVisited 0 m
        (right, rVisited) = traverse mVisited 0 r
        total = left + middle + right
        newVisited = (value, total):rVisited
    traverse visited acc Leaf = (acc, visited)

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let joltages = read <$> lines contents
  let part1Sol = (findDifferences 1 joltages) * (findDifferences 3 joltages)
  putStrLn $ "Number of 1 jolt differences multiplied by number of 3 jolt differences (Part 1): " ++ (show part1Sol)
  let tree = constructTree joltages
  putStrLn $ "Number of possible adapter combinations (Part 2): " ++ (show $ findNumCombinations joltages)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
