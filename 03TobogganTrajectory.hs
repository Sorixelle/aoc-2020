import Data.List
import System.Environment

countTrees :: (Int, Int) -> String -> (Int, Int)
countTrees (prevPos, trees) s
  | char == '.' = (pos, trees)
  | char == '#' = (pos, trees + 1)
  where
    pos = mod (prevPos + 3) (length s)
    char = s !! pos

travelSlope :: [String] -> Int
travelSlope s = snd $ foldl' countTrees (0, 0) (tail s)

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let map = lines contents
  putStrLn $ "Number of trees hit: " ++ (show $ travelSlope map)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
