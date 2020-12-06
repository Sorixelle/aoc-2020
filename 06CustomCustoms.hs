import Data.List
import System.Environment

splitAtEmptyLine :: [String] -> String -> [String]
splitAtEmptyLine xs "" = "" : xs
splitAtEmptyLine (x:xs) s = (x ++ s) : xs
splitAtEmptyLine xs s = [s]

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let groups = foldl' splitAtEmptyLine [] $ lines contents
  let sumOfAnswers = sum $ (length . nub) <$> groups
  putStrLn $ "Sum of amount of answers (Part 1): " ++ (show sumOfAnswers)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
