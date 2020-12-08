import System.Environment

parseInstruction :: String -> (String, Int)
parseInstruction line = (opcode, num)
  where
    (opcode:num') = words line
    num = case head num' of
      '+':n -> read n
      '-':n -> read $ head num'

runProg :: [(String, Int)] -> (String, Int)
runProg program = loop [] 0 0
  where
    loop :: [Int] -> Int -> Int -> (String, Int)
    loop visited idx acc
      | idx `elem` visited   = ("fail", acc)
      | idx >= length program = ("end", acc)
      | otherwise            = case opcode of
                                 "acc"  -> loop visited' (idx + 1) (acc + val)
                                 "jmp"  -> loop visited' (idx + val) acc
                                 "nop"  -> loop visited' (idx + 1) acc
                                 "fail" -> ("fail", acc)
        where
          (opcode, val)
            | idx > (length program) - 1 = ("fail", 0)
            | otherwise                  = program !! idx
          visited' = idx:visited

findInstCausingLoop :: [(String, Int)] -> Int
findInstCausingLoop program = loop [] 0 0
  where
    loop :: [Int] -> Int -> Int -> Int
    loop visited distance idx
      | idx `elem` visited = case fst $ patchedResult (visited !! distance) of
                               "fail" -> loop visited (distance + 1) idx
                               "end"  -> visited !! distance
      | otherwise          = case opcode of
                               "jmp" -> loop visited' distance (idx + val)
                               _     -> loop visited' distance (idx + 1)
      where
        (opcode, val)
          | idx > (length program) - 1 = ("fail", 0)
          | otherwise                  = program !! idx
        visited' = idx:visited
        patchedResult i = runProg $ patchProgram program i


patchProgram :: [(String, Int)] -> Int -> [(String, Int)]
patchProgram program pos = head ++ ((newOpcode, value):tail)
  where
    (head, ((opcode, value):tail)) = splitAt pos program
    newOpcode = case opcode of
                  "jmp" -> "nop"
                  "nop" -> "jmp"
                  x     -> x

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let program = parseInstruction <$> lines contents
  putStrLn $ "Accumulator at point of infinite loop (Part 1): " ++ (show $ snd $ runProg program)
  let patchedProgram = patchProgram program (findInstCausingLoop program)
  putStrLn $ "Accumulator at termination of patched program (Part 2): " ++ (show $ snd $ runProg patchedProgram)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
