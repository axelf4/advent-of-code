import Control.Monad (foldM)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main = do
  expenses <- readFile "input"
    >>= (return . (map read) . lines)

  -- Keep track of potential expenses that would add up to 2020 with
  -- an already seen one.
  let match = foldM f Set.empty expenses
        where
          f needles x =
            if Set.member x needles then Left x
            else Right $ Set.insert (2020 - x) needles

  putStrLn (case match of
    Left x -> "Product of the two entries that sum to 2020: " ++ show (x * (2020 - x))
    Right _ -> "Did not find the pair!")

  let match2 = foldM f (Set.empty, Map.empty) (filter (<2020) expenses)
        where
          f :: (Set.Set Int, Map.Map Int Int) -> Int -> Either (Int, Int) (Set.Set Int, Map.Map Int Int)
          f (alreadySeen, needles) x =
            case Map.lookup x needles of
              Just a -> Left (a, x)
              _ -> Right ((Set.insert x alreadySeen), (addPotentialNewMatches alreadySeen needles x))
          addPotentialNewMatches :: (Set.Set Int) -> (Map.Map Int Int) -> Int -> (Map.Map Int Int)
          addPotentialNewMatches alreadySeen needles x =
            Set.foldl' (\m a -> Map.insert (2020 - a - x) a m) needles alreadySeen

  putStrLn (case match2 of
    Left (a, x) -> "Product of the three entries that sum to 2020: " ++ show (a * x * (2020 - a - x))
    Right _ -> "Did not find triplet!")
