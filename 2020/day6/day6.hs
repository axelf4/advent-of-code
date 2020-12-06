import qualified Data.Set as S
import Data.List (foldl1')

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn sep s = case break (==sep) s of
  (a, sep : b) -> a : splitOn sep b
  (a, []) -> [a]

distinctAnswers :: [String] -> Int
distinctAnswers xs =
  S.size $ S.fromList (concat xs)

  
intersectionAnswers :: [String] -> Int
intersectionAnswers xs =
  S.size $ foldl1' S.intersection (map S.fromList xs)

main = do
  lines <- lines <$> readFile "input"

  let groups = splitOn "" lines

  putStrLn $ "Sum of group yes:es is "
    ++ show (sum $ distinctAnswers <$> groups)

  putStrLn $ "Sum of group yes intersections is "
    ++ show (sum $ intersectionAnswers <$> groups)
