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

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let specs = lines contents
  let highestID = foldr max 0 $ seatSpecToID <$> specs
  putStrLn $ "Highest seat ID (Part 1): " ++ (show highestID)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
