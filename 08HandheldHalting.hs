import System.Environment

parseInstruction :: String -> (String, Int)
parseInstruction line = (opcode, num)
  where
    (opcode:num') = words line
    num = case head num' of
      '+':n -> read n
      '-':n -> read $ head num'

runProgTillInfLoop :: [(String, Int)] -> Int
runProgTillInfLoop program = loop [] 0 0
  where
    loop :: [Int] -> Int -> Int -> Int
    loop visited idx acc
      | idx `elem` visited = acc
      | otherwise          = case opcode of
                               "acc" -> loop visited' (idx + 1) (acc + val)
                               "jmp" -> loop visited' (idx + val) acc
                               "nop" -> loop visited' (idx + 1) acc
        where
          (opcode, val) = program !! idx
          visited' = idx:visited

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let program = parseInstruction <$> lines contents
  putStrLn $ "Accumulator at point of infinite loop (Part 1): " ++ (show $ runProgTillInfLoop program)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
