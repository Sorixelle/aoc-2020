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
  putStrLn $ "Result: " ++ (show $ findResult fileLines)

findResult :: [String] -> Int
findResult = either id id . findResult'

findResult' :: [String] -> Either Int Int
findResult' strings =
  foldM f 0 pairs
  where
    numbers = map (\s -> read s :: Int) strings
    pairs = sequence $ replicate 2 numbers
    f acc p
      | (sum p) == 2020 = Left (product p)
      | otherwise       = Right acc
