import Control.Monad (void)
import Data.Char (digitToInt)
import Text.Parsec (ParseError, parse, (<|>), char, digit, spaces, eof, chainl1)
import Text.Parsec.String (Parser)

parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* char ')' <* spaces

addOp = char '+' *> spaces *> pure (+)
mulOp = char '*' *> spaces *> pure (*)

eval1 :: String -> Int
eval1 = either (error . show) id . parse (expr <* eof) ""
  where
    atom = (digitToInt <$> digit <* spaces) <|> parens expr
    op = addOp <|> mulOp
    expr = atom `chainl1` op

eval2 :: String -> Int
eval2 = either (error . show) id . parse (expr <* eof) ""
  where
    expr = factor `chainl1` mulOp
    factor = term `chainl1` addOp
    term = (digitToInt <$> digit <* spaces) <|> parens expr

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let result = sum $ eval2 <$> input
  putStrLn $ show result
