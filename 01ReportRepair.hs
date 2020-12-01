import Control.Monad
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
findResult pairLength = either id id . (findResult' pairLength)

findResult' :: Int -> [String] -> Either Int Int
findResult' pairLength strings =
  foldM f 0 pairs
  where
    numbers = map (\s -> read s :: Int) strings
    pairs = sequence $ replicate pairLength numbers
    f acc p
      | (sum p) == 2020 = Left (product p)
      | otherwise       = Right acc
