import qualified Data.List as List
import Data.List (foldl', find, intercalate, sortOn)
import Data.Maybe (fromJust)
import Text.Parsec (char, string, eof, many1, sepBy1, sepEndBy1, between)
import Text.Parsec.Char (endOfLine, letter)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.SBV (AllSatResult(..), getModelValue, Predicate, SBool, allSat, sBool, sAnd, sOr, (.&&), pbMutexed, pbStronglyMutexed)

type Ingredient = String
type Allergen = String

data Food = Food
  { ingredients :: [Ingredient]
  , allergens :: [Allergen]
  }
  deriving (Show)

parseInput :: String -> IO [Food]
parseInput f = either (error . show) id <$> parseFromFile (parser <* eof) f
  where
    parser = food `sepEndBy1` endOfLine
    parens = between (char '(') (char ')')
    word = many1 letter
    food = do
      ingredients <- word `sepEndBy1` char ' '
      allergens <- parens $ string "contains " *> (word `sepBy1` string ", ")
      pure (Food { ingredients = ingredients, allergens = allergens })

main :: IO ()
main = do
  foods <- parseInput "input"
  let
    allIngredients = Set.toList $ Set.fromList
      $ foods >>= \(Food { ingredients = is }) -> is
    allAllergens = Set.toList $ Set.fromList
      $ foods >>= \(Food { allergens = as }) -> as

    ingredientAllergenPairs = Set.fromList
      $ (,) <$> allIngredients <*> allAllergens

    predicate :: Predicate
    predicate = do
      vars <- Map.fromList <$> mapM
        (\(i, a) -> (,) (i, a) <$> sBool (i <> "-" <> a))
        (Set.toList ingredientAllergenPairs)

      let
        allergenUnique = sAnd
            $ (\a -> pbStronglyMutexed ((\i -> vars Map.! (i, a)) <$> allIngredients)) <$> allAllergens
        ingredientMaybeAllergen = sAnd
          $ (\i -> pbMutexed ((\a -> vars Map.! (i, a)) <$> allAllergens)) <$> allIngredients

      pure $ allergenUnique .&& ingredientMaybeAllergen .&& (sAnd
        $ (\(Food { ingredients = is, allergens = as }) ->
             sAnd $ (\a -> sOr $ (\i -> vars Map.! (i, a)) <$> is) <$> as)
        <$> foods)

  allSatResult@(AllSatResult { allSatResults = results }) <- allSat predicate

  let neverAllergen = foldl'
        (\acc res -> Set.filter
          (\i ->
             not $ any (\a -> fromJust $ getModelValue (i <> "-" <> a) res) allAllergens)
          acc)
        (Set.fromList allIngredients)
        results

      numAppearances = length
        $ filter (`Set.member` neverAllergen)
        $ foods >>= (\(Food { ingredients = is}) -> is)

      ingredientAllergen i = fromJust
        $ find (\a -> fromJust $ getModelValue (i <> "-" <> a) (head results)) allAllergens

      dangerousList = intercalate ","
        $ sortOn ingredientAllergen
        $ filter (`Set.notMember` neverAllergen) allIngredients

  putStrLn $ show neverAllergen
  putStrLn $ show numAppearances
  putStrLn $ show dangerousList
