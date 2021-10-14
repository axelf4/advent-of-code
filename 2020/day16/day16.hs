import Text.Parsec (ParseError, parse, char, string, (<|>), many, many1, count, letter, digit, try, spaces, space, endOfLine, eof, sepBy1)
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (find, transpose, isPrefixOf)
import Data.SBV (SatResult(..), SMTResult(Satisfiable), getModelValue, Predicate, SBool, sat, sBool, sNot, sAnd, sOr, (.&&), pbStronglyMutexed)

data Range = Range Int Int
  deriving (Show, Eq, Ord)

data Field = Field String Range Range
  deriving (Show, Eq, Ord)

type Ticket = [Int]

data Notes = Notes { fields :: [Field], yourTicket :: Ticket, nearbyTickets :: [Ticket] }
  deriving (Show)

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

parseInput :: String -> Either ParseError Notes
parseInput = parse parser ""
  where
    number :: Parser Int
    number = read <$> many1 digit

    range = do
      a <- number
      void $ char '-'
      b <- number
      return $ Range a b

    field = do
      fieldName <- many1 (letter <|> char ' ')
      void $ string ": "

      range1 <- range <* space
      void $ string "or" <* space
      range2 <- range

      void endOfLine

      return $ Field fieldName range1 range2

    ticket = sepBy1 number (char ',') <* endOfLine

    parser = do
      fields <- many1 field

      void $ endOfLine *> string "your ticket:" *> endOfLine
      yourTicket <- ticket

      void $ endOfLine *> string "nearby tickets:" *> endOfLine
      nearbyTickets <- many ticket

      void eof

      return $ Notes { fields = fields, yourTicket = yourTicket, nearbyTickets = nearbyTickets }

inRange :: Range -> Int -> Bool
inRange (Range min max) n = min <= n && n <= max

main :: IO ()
main = do
  input <- readFile "input"
  let notes@Notes { fields = fields, yourTicket = yourTicket, nearbyTickets = nearbyTickets }
        = either (error . show) id $ parseInput input
  putStrLn $ show notes

  let ranges :: [Range]
      ranges = fields >>= \(Field _ a b) -> [a, b]

      valid :: Int -> Bool
      valid value = any (flip inRange $ value) ranges

      errorRate = let
          values = concat nearbyTickets
        in sum $ filter (not . valid) values

      validNearby :: [[Int]]
      validNearby = filter (all valid) nearbyTickets

      predicate :: Predicate
      predicate = do
        let columnIndices = [0..length fields - 1]

        vars <- mapM
                (\(Field name _ _) -> mapM
                       (\j -> sBool $ name <> show j)
                       columnIndices)
                fields

        let allFieldsHaveOneCol = sAnd $ pbStronglyMutexed <$> vars
            oneFieldPerCol = sAnd $ pbStronglyMutexed <$> transpose vars
            rangeCriteria = sAnd
              $ (\ticket ->
                 sAnd $ zipWith
                   (\(Field _ a b) col ->
                      sAnd $ catMaybes $ zipWith (\value var ->
                                 if inRange a value || inRange b value
                                 then Nothing
                                 else Just $ sNot var) ticket col)
                   fields vars)
              <$> validNearby

        pure $ allFieldsHaveOneCol .&& oneFieldPerCol .&& rangeCriteria

  putStrLn $ "Answer to part 1: " <> show errorRate

  satResult@(SatResult result@(Satisfiable _cfg _model)) <- sat predicate
  putStrLn $ show satResult

  let answer =
        product
        [ let Just (_, value) =
                find
                (\(i, v) -> fromMaybe False
                            $ getModelValue (name <> show i) result)
                $ zip [0..] yourTicket
          in value
        | (Field name _ _) <- fields
        , "departure" `isPrefixOf` name
        ]
  putStrLn $ "Answer to part 2: " <> show answer
