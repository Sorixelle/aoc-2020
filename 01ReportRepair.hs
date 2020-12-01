import System.Environment

main :: IO ()
main = do
  args <- getArgs
  run $ head args

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  putStrLn $ "Part 1 Result: " ++ (show $ findResult 2 fileLines)
  putStrLn $ "Part 2 Result: " ++ (show $ findResult 3 fileLines)

findResult :: Int -> [String] -> Int
findResult pairLength strings =
  findResult' pairs
  where
    numbers = map (\s -> read s :: Int) strings
    pairs = sequence $ replicate pairLength numbers

findResult' :: [[Int]] -> Int
findResult' (x:xs)
  | sum x == 2020 = product x
  | otherwise     = findResult' xs
