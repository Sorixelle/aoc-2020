import Data.List
import System.Environment

data Ship = Ship Int Int Int deriving Show
data Waypoint = Waypoint Int Int deriving Show

followRoute :: Ship -> String -> Ship
followRoute ship@(Ship x y r) instruction = case head dir of
  'N' -> Ship x (y + amt) r
  'S' -> Ship x (y - amt) r
  'E' -> Ship (x + amt) y r
  'W' -> Ship (x - amt) y r
  'F' -> followRoute ship ((degToDirection r):val)
  'L' -> Ship x y $ rotate (-amt)
  'R' -> Ship x y $ rotate amt
  where
    (dir, val) = splitAt 1 instruction
    amt = read val
    rotate d = abs $ (r + d) `mod` 360
    degToDirection deg
      | deg == 0   = 'N'
      | deg == 90  = 'E'
      | deg == 180 = 'S'
      | deg == 270 = 'W'

followWaypoint :: (Waypoint, Ship) -> String -> (Waypoint, Ship)
followWaypoint (wp@(Waypoint wx wy), ship@(Ship x y r)) instruction = case head dir of
  'N' -> (Waypoint wx (wy + amt), ship)
  'S' -> (Waypoint wx (wy - amt), ship)
  'E' -> (Waypoint (wx + amt) wy, ship)
  'W' -> (Waypoint (wx - amt) wy, ship)
  'F' -> (wp, Ship (x + (wx * amt)) (y + (wy * amt)) r)
  'L' -> (rotateWaypoint wp (-amt), ship)
  'R' -> (rotateWaypoint wp amt, ship)
  where
    (dir, val) = splitAt 1 instruction
    amt = read val
    rotateWaypoint (Waypoint x y) r
      | deg == 0   = Waypoint x y
      | deg == 90  = Waypoint y (-x)
      | deg == 180 = Waypoint (-x) (-y)
      | deg == 270 = Waypoint (-y) x
      where
        deg = abs $ (360 + r) `mod` 360

manhattanDistance :: Ship -> Int
manhattanDistance (Ship x y _) = (abs x) + (abs y)

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let instructions = lines contents
  let finalPosP1 = foldl' followRoute (Ship 0 0 90) instructions
  putStrLn $ "Manhattan distance from ship starting location (Part 1 rules): " ++ (show $ manhattanDistance finalPosP1)
  let finalPosP2 = snd $ foldl' followWaypoint (Waypoint 10 1, Ship 0 0 0) instructions
  putStrLn $ "Manhattan distance from ship starting location (Part 2 rules): " ++ (show $ manhattanDistance finalPosP2)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
