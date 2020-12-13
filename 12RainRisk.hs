import Data.List
import System.Environment

data Ship = Ship Int Int Int deriving Show

degToDirection :: Int -> Char
degToDirection deg
  | deg == 0   = 'N'
  | deg == 90  = 'E'
  | deg == 180 = 'S'
  | deg == 270 = 'W'

followRoute :: Ship -> String -> Ship
followRoute ship@(Ship x y r) instruction = case head dir of
  'N' -> Ship x (y + amt) r
  'S' -> Ship x (y - amt) r
  'E' -> Ship (x + amt) y r
  'W' -> Ship (x - amt) y r
  'F' -> followRoute ship ((degToDirection r):val)
  'L' -> Ship x y $ rotateCCW amt
  'R' -> Ship x y $ rotateCW amt
  where
    (dir, val) = splitAt 1 instruction
    amt = read val
    rotateCW d = abs $ (r + d) `mod` 360
    rotateCCW d = abs $ (r - d) `mod` 360

manhattanDistance :: Ship -> Int
manhattanDistance (Ship x y _) = (abs x) + (abs y)

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let instructions = lines contents
  let finalPos = foldl' followRoute (Ship 0 0 90) instructions
  putStrLn $ "Manhattan distance from ship starting location (Part 1): " ++ (show $ manhattanDistance finalPos)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
