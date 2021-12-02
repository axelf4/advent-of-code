import Control.Monad (void)
import Data.Maybe (isJust)
import Text.Parsec (ParseError, parse, (<|>), char, digit, eof, many, many1, chainr1)
import Text.Parsec.Char (oneOf, anyChar, endOfLine, letter)
import Text.Parsec.String (Parser)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Text.Regex.PCRE.String as Regex
import Text.Regex.PCRE.String (Regex, regexec)

parseInput :: String -> (String, [String])
parseInput s = ("^" <> rules ! 0 <> "$", messages)
  where
    (rules, messages) = either (error . show) id
      $ parse (parser <* eof) "" s

    spaces = many $ oneOf " \t\f"

    atom = (: "") <$> (char '"' *> anyChar <* char '"' <* spaces)
    ruleRef = do
      i <- read <$> many1 digit
      void spaces
      pure $ case i of
        8 -> (rules ! 42) <> "+"
        11 -> "(" <> (rules ! 42) <> "(?1)?" <> (rules ! 31) <> ")"
        _ -> rules ! i
    subrule = concat <$> many1 ruleRef
    altOp = char '|' *> spaces *> pure (\a b -> a <> ('|' : b))
    rule = atom <|> ((\pat -> "(?:" <> pat <> ")") <$> subrule `chainr1` altOp)

    ruleNumber = read <$> many1 digit <* char ':' <* spaces
    parser = do
      rules <- Map.fromList
        <$> many1 ((,) <$> ruleNumber <*> rule <* endOfLine)
      void endOfLine
      messages <- many1 (many1 letter <* endOfLine)
      pure (rules, messages)

main = do
  input <- readFile "input"
  let (rule0, messages) = parseInput input
  Right regex <- Regex.compile Regex.compDollarEndOnly Regex.execBlank rule0

  matching <- mapM (regexec regex) messages
  let numMatching = length
        $ filter (isJust . either (error . show) id) matching
  putStrLn $ show numMatching
