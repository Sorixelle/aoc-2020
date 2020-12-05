import Data.List
import System.Environment

binarySearch :: (Char, Char) -> (Double, Double) -> String -> Int
binarySearch (upperChar, lowerChar) (min, max) = round . run min max
  where
    run :: Double -> Double -> String -> Double
    run start end (c:[])
      | c == lowerChar = start
      | c == upperChar = end
    run start end (c:xs)
      | c == lowerChar = run start (end - diff) xs
      | c == upperChar = run (start + diff) end xs
      where
        diff = fromIntegral $ ceiling $ (end - start) / 2

findRow :: String -> Int
findRow = binarySearch ('B', 'F') (0, 127)

findCol :: String -> Int
findCol = binarySearch ('R', 'L') (0, 7)

seatSpecToID :: String -> Int
seatSpecToID spec = findRow rowSpec * 8 + findCol colSpec
  where
    (rowSpec, colSpec) = splitAt 7 spec

findMySeatID :: [Int] -> Int
findMySeatID (c:n:xs)
  | c + 1 /= n = c + 1
  | otherwise  = findMySeatID $ n:xs
findMySeatID _ = 0

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let seatIDs = seatSpecToID <$> lines contents
  let highestID = foldr max 0 seatIDs
  putStrLn $ "Highest seat ID (Part 1): " ++ (show highestID)
  putStrLn $ "My seat ID (Part 2): " ++ (show $ findMySeatID $ sort seatIDs)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
