import Debug.Trace
import Data.Matrix hiding (trace)
import Data.Maybe
import System.Environment

updateSeat :: Matrix Char -> (Int, Int) -> Char -> Char
updateSeat seats idx seat
  | seat == 'L' && occupied == 0 = '#'
  | seat == 'L' && occupied /= 0 = 'L'
  | seat == '#' && occupied >= 4 = 'L'
  | seat == '#' && occupied < 4  = '#'
  | seat == '.'                  = '.'
  where
    occupied = length $ filter id $ check <$> toCheck
    toCheck = filter (/= idx) $ do
      y <- [fst idx - 1..fst idx + 1]
      x <- [snd idx - 1..snd idx + 1]
      return (y, x)
    check (y, x) = case s of
      Just '#' -> True
      _        -> False
      where
        s = safeGet y x seats

findConstantPlan :: Matrix Char -> Matrix Char
findConstantPlan seats = loop seats $ update seats
  where
    update s = mapPos (updateSeat s) s
    loop old new
      | old == new = new
      | otherwise  = loop new $ update new

totalOccupied :: Matrix Char -> Int
totalOccupied = length . filter (== '#') . toList

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let seatPlan = fromLists $ lines contents
  putStrLn $ "Number of occupied seats at constant plan (Part 1): " ++ (show $ totalOccupied $ findConstantPlan seatPlan)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
