import Data.Char (isDigit, ord)
import Text.Read (readMaybe)
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map.Strict as M

data FieldType = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid  
  deriving (Show, Eq, Ord)

parseFieldType :: String -> FieldType
parseFieldType s = case s of
  "byr" -> Byr; "iyr" -> Iyr; "eyr" -> Eyr; "hgt" -> Hgt
  "hcl" -> Hcl; "ecl" -> Ecl; "pid" -> Pid; "cid" -> Cid
  
lexeme :: Parser a -> Parser a  
lexeme parser = parser <* spaces

newtype Passport = Passport (M.Map FieldType String)

parsePassport :: String -> Either ParseError Passport
parsePassport = parse passport ""
  where
    field = lexeme (do
      key <- many1 letter
      char ':'
      value <- many1 (alphaNum <|> char '#')
      return (parseFieldType key, value))
    passport = do
      fields <- many1 field
      return $ Passport (M.fromList fields)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn sep s = case break (==sep) s of
  (a, sep : b) -> a : splitOn sep b
  (a, []) -> [a]

parseInput :: IO (Either ParseError [Passport])
parseInput = do
  lines <- lines <$> readFile "input"

  -- Passports are separeted by an empty line
  return $ sequence (parsePassport <$> unlines <$> splitOn "" lines)

validField :: (FieldType, String) -> Bool
validField (k, v) = case k of
    Byr -> isBetween 1920 2002 v
    Iyr -> isBetween 2010 2020 v
    Eyr -> isBetween 2020 2030 v
    Hgt -> case span isDigit v of
        (n, "cm") -> isBetween 150 193 n
        (n, "in") -> isBetween 59 76 n
        _ -> False
    Hcl -> case v of
      '#' : xs -> length xs == 6
        && all (\c -> isDigit c || (fromIntegral (ord c - ord 'a')::Word) <= 5) xs
      _ -> False
    Ecl -> case v of
      "amb" -> True
      "blu" -> True
      "brn" -> True
      "gry" -> True
      "grn" -> True
      "hzl" -> True
      "oth" -> True
      _ -> False
    Pid -> length v == 9 && all isDigit v
    -- Ignored, missing or not
    Cid -> True
  where
    isBetween a b = (maybe False (\n -> a <= n && n <= b)) . readMaybe

valid :: Passport -> Bool
valid (Passport map)
  = let hasCid = M.member Cid map
    in M.size map == (if hasCid then 8 else 7)
       && all validField (M.assocs map)

main = do
  passports <- (parseInput >>= (\x -> case x of
                                        Right x -> return x
                                        Left e -> error $ "Failed to parse: " ++ show e))

  let numValid = length $ filter valid passports

  putStrLn $ "Num valid: " ++ show numValid


