import Debug.Trace
import Data.List
import System.Environment

countTrees :: Int -> Int -> (Int, Int, Int) -> String -> (Int, Int, Int)
countTrees right down (prevPos, trees, idx) s
  | mod idx down == 0 && char == '#' = (pos, trees + 1, idx + 1)
  | mod idx down /= 0                = (prevPos, trees, idx + 1)
  | otherwise                        = (pos, trees, idx + 1)
  where
    pos = mod (prevPos + right) (length s)
    char = s !! pos

travelSlope :: [String] -> (Int, Int) -> Int
travelSlope s (right, down) =
  let
    (_, trees, _) = foldl' (countTrees right down) (0, 0, 1) (tail s)
  in trees

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let treeMap = lines contents
  putStrLn $ "Number of trees hit (Part 1): " ++ (show $ travelSlope treeMap (3,1))
  let prodSlopes = product $ map (travelSlope treeMap) [(1,1), (3,1), (5,1), (7,1), (1,2)]
  putStrLn $ "Product of trees hit on all slopes (Part 2): " ++ show prodSlopes

main :: IO ()
main = do
  args <- getArgs
  run $ head args
