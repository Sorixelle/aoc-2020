import Data.List
import System.Environment

splitAtEmptyLine :: [String] -> String -> [String]
splitAtEmptyLine xs "" = "" : xs
splitAtEmptyLine ("":xs) s = s : xs
splitAtEmptyLine (x:xs) s = (x ++ " " ++ s) : xs
splitAtEmptyLine xs s = [s]

findCommonAnswers :: String -> String
findCommonAnswers xs = foldl1 intersect $ nub <$> words xs

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let groups = foldl' splitAtEmptyLine [] $ lines contents
  let sumOfAnswers = sum $ (length . nub) <$> filter (/= ' ') <$> groups
  putStrLn $ "Sum of amount of answers (Part 1): " ++ (show sumOfAnswers)
  let sumOfCommonAnswers = sum $ length <$> findCommonAnswers <$> groups
  putStrLn $ "Sum of group common answers (Part 2): " ++ (show sumOfCommonAnswers)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
