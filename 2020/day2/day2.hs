import Text.Parsec
import Text.Parsec.String

data Policy = Policy { a :: Int, b :: Int, l :: Char }

lexeme :: Parser a -> Parser a  
lexeme parser = parser <* spaces
  
parseLine :: String -> Either ParseError (Policy, String)
parseLine = parse passPair "input"
  where
    decimal = lexeme $ read <$> many1 digit
    policy = do
      min <- decimal
      char '-'
      max <- decimal
      letter <- lexeme $ letter
      return Policy { a = min, b = max, l = letter }
    password = many1 letter
    passPair = do
      policy <- policy
      string ": "
      password <- password
      return (policy, password)

valid1 :: (Policy, String) -> Bool
valid1 (Policy { a=a, b=b, l=l }, s)
  = let count = length $ filter (==l) s
    in a <= count && count <= b
 
xor a b = a /= b  

valid2 :: (Policy, String) -> Bool
valid2 (Policy { a=a, b=b, l=l }, s)
  = (s !! (a-1) == l) `xor` (s !! (b-1) == l)

main = do
  lines <- lines <$> readFile "input"
  let pairs = case sequence $ parseLine <$> lines of
                Left e -> error $ "Failed to parse: " ++ show e
                Right x -> x

  putStrLn $ "Number of valid: "
    ++ show (length $ filter valid1 pairs)

  putStrLn $ "Number of valid: "
    ++ show (length $ filter valid2 pairs)
