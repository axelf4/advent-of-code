import Data.List (foldl')
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

parseInput :: String -> Either ParseError (M.Map String [(Int, String)])
parseInput input = M.fromList <$> (parse rules "" input)
  where
    word = many1 letter
    color = concat <$> sequence [word, string " ", word]
    bagAmount = do
      count <- read <$> many1 digit <* spaces
      bagColor <- color
      string " bag"
      optional (char 's')
      return (count, bagColor)
    rule = do
      c <- color
      string " bags contain" <* spaces
      contents <- choice
        [ bagAmount `sepBy1` (string ", ")
        , (const []) <$> string "no other bags"
        ] <* (char '.')
      return (c, contents)
    rules = (rule `sepEndBy` (char '\n')) <* eof

canContain :: String -> M.Map String [(Int, String)] -> Int
canContain needle rules = length $ filter id (M.elems canContain)
  where
    canContain = M.fromList [(k, go contents) | (k, contents) <- M.toList rules]
    go = any (\(_, k) -> if k == needle then True
                                  else canContain M.! k)

nestedCount :: String -> M.Map String [(Int, String)] -> Int
nestedCount s rules = sum ((\(n, k) -> n * (1 + nestedCount k rules)) <$> contents)
  where contents = rules M.! s

main = do
  input <- readFile "input"

  let rules = case parseInput input of
        Left e -> error $ show e
        Right x -> x

  putStrLn $ show (canContain "shiny gold" rules)

  putStrLn $ show (nestedCount "shiny gold" rules)
