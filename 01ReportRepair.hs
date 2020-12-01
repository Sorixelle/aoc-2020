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
  foldM f 0 numbers
  where
    numbers = map (\s -> read s :: Int) strings
    f _ n = foldM (f' n) 0 numbers
    f' cur _ n
      | cur + n == 2020 = Left (cur * n)
      | otherwise       = Right 0
