import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

type Quantity = Int
type Chemical = String
type Component = (Quantity, Chemical)
type Reaction = ([Component], Component)
type Reactions = Map.Map Chemical (Quantity, [Component])

componentsForMany :: Int -> [Component] -> [Component]
componentsForMany n = map (\(quantity, chem) -> (n * quantity, chem))

constituents :: Reactions -> Chemical -> (Int, [Component])
constituents rs chem = fromJust $ Map.lookup chem rs

oreForOneFuel :: Reactions -> Int
oreForOneFuel rs = go [(1, "FUEL")] Map.empty 0
    -- If backlog empty: Return necessary ore amount
  where
    go [] _ oreCount = oreCount
    -- Count required OREs
    go ((count, "ORE"):backlog) available prevOreCount =
      go backlog available (prevOreCount + count)
    -- Skip if no count
    go ((count, _):backlog) available oreCount
      | count <= 0 = go backlog available oreCount
    -- We need to produce one `chem`
    go ((count, chem):backlog) available oreCount =
      let (exist, available') =
            Map.updateLookupWithKey (\k a -> Just (a - 1)) chem available
       in case exist of
            Just c
              | c >= 0 -> go ((count - 1, chem) : backlog) available' oreCount
            _ ->
              let (quantity, components) = constituents rs chem
               in go
                    ((count - 1, chem) : components ++ backlog)
                    (Map.insert chem (quantity - 1) available')
                    oreCount

readReactions :: IO [Reaction]
readReactions = do
  lines <- lines <$> readFile "nanofactoryInput.txt"
  let reactions = ((\x -> case x of Right reaction -> reaction
        ) . parseReaction) <$> lines
  return reactions
  where
    lexeme :: Parser a -> Parser a
    lexeme parser = parser <* spaces
    chemical = lexeme $ many1 letter
    decimal = lexeme $ read <$> many1 digit
    component = do
      quantity <- decimal
      chem <- chemical
      return (quantity, chem)
    commaSep parser = parser `sepBy` (lexeme $ char ',')
    reaction = do
      constituents <- commaSep component
      lexeme $ string "=>"
      result <- component
      return (constituents, result)
    parseReaction = parse reaction ""

main = do
  reactions <- readReactions
  let reactions' = Map.fromList $ map (\(constituents, (quantity, chem)) -> (chem, (quantity, constituents))) reactions
  putStrLn (show reactions')
  putStrLn (show $ oreForOneFuel reactions')
