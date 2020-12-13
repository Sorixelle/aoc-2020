import Data.Matrix
import Data.Maybe
import System.Environment

type Checker = Matrix Char -> (Int, Int) -> [(Int, Int)]

checkAdjacent :: Checker
checkAdjacent _ idx = do
  y <- [fst idx - 1..fst idx + 1]
  x <- [snd idx - 1..snd idx + 1]
  return (y, x)

data TraceState = Cont | Stop | None deriving Show

checkLineOfSight :: Checker
checkLineOfSight seats idx = do
  y <- [-1..1]
  x <- [-1..1]
  return $ traceLine y x
  where
    check y x = case safeGet y x seats of
      Just '.' -> Cont
      Nothing  -> None
      _        -> Stop
    traceLine 0 0 = idx
    traceLine dirY dirX = loop idx
      where
        loop (y', x') = case check (dirY + y') (dirX + x') of
          Cont -> loop (dirY + y', dirX + x')
          Stop -> (dirY + y', dirX + x')
          None -> idx

updateSeat :: Matrix Char -> Int -> Checker -> (Int, Int) -> Char -> Char
updateSeat seats thresh checker idx seat
  | seat == 'L' && occupied == 0      = '#'
  | seat == 'L' && occupied /= 0      = 'L'
  | seat == '#' && occupied >= thresh = 'L'
  | seat == '#' && occupied < thresh  = '#'
  | seat == '.'                       = '.'
  where
    occupied = length $ filter id $ check <$> (filter (/= idx) $ checker seats idx)
    check (y, x) = case s of
      Just '#' -> True
      _        -> False
      where
        s = safeGet y x seats

findConstantPlan :: Matrix Char -> Int -> Checker -> Matrix Char
findConstantPlan seats thresh checker = loop seats $ update seats
  where
    update s = mapPos (updateSeat s thresh checker) s
    loop old new
      | old == new = new
      | otherwise  = loop new $ update new

totalOccupied :: Matrix Char -> Int
totalOccupied = length . filter (== '#') . toList

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let seatPlan = fromLists $ lines contents
      finalSeats t = totalOccupied . findConstantPlan seatPlan t
  putStrLn $ "Number of occupied seats at constant plan (Part 1 rules): " ++ (show $ finalSeats 4 checkAdjacent)
  putStrLn $ "Number of occupied seats at constant plan (Part 2 rules): " ++ (show $ finalSeats 5 checkLineOfSight)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
