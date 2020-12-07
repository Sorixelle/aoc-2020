import Data.Either
import Data.List
import System.Environment
import Text.Parsec

word :: Parsec String () String
word = manyTill letter space

bagKey :: Parsec String () String
bagKey = do
  mod <- word
  colour <- word
  return $ mod ++ " " ++ colour

bagValue :: Parsec String () (Int, String)
bagValue = do
  num <- digit
  _ <- space
  bag <- bagKey
  _ <- string "bag"
  _ <- optional $ char 's'
  return (read [num], bag)

noOtherBags :: Parsec String () [(Int, String)]
noOtherBags = do
  _ <- string "no other bags"
  return []

bagRule :: Parsec String () (String, [(Int, String)])
bagRule = do
  key <- bagKey
  _ <- string "bags contain "
  values <- (try $ bagValue `sepBy1` string ", ") <|> noOtherBags
  _ <- char '.'
  return (key, values)

containsBag :: String -> [(String, [(Int, String)])] -> [String]
containsBag colour xs = fst <$> filter f xs
  where
    f (_, rules) = colour `elem` (snd <$> rules)

searchBag :: [(String, [(Int, String)])] -> String -> [String]
searchBag rules colour = searchBag' rules [colour]
  where
    searchBag' :: [(String, [(Int, String)])] -> [String] -> [String]
    searchBag' rules colours
      | newSpace == colours = colours
      | otherwise           = nub $ (searchBag' rules newSpace) ++ newSpace
      where
        newSpace = nub $ colours >>= (\c -> containsBag c rules)

run :: String -> IO ()
run fileName = do
  contents <- readFile fileName
  let rules = fromRight ("",[]) . parse bagRule "" <$> lines contents
  let numOfP1ValidBags = length $ searchBag rules "shiny gold"
  putStrLn $ "Number of bags that can contain a shiny gold bag (Part 1): " ++ (show numOfP1ValidBags)

main :: IO ()
main = do
  args <- getArgs
  run $ head args
