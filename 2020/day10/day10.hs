{-# LANGUAGE TupleSections #-}

import Data.List (sort, mapAccumL, group)
import qualified Data.Map.Strict as M

frequency = (M.fromListWith (+)) . (map (, 1))

main = do
  adapters <- (sort . (read<$>) . lines) <$> readFile "input"

  let deviceJoltage = (maximum adapters) + 3

  let diffs = snd $ mapAccumL (\a b -> (b, b - a)) 0 (adapters ++ [deviceJoltage])

  -- Part One
  let diffCounts = frequency diffs
  putStrLn $ show ((diffCounts M.! 1) * (diffCounts M.! 3))

  -- Part Two
  let test = product $ map (\xs -> let d = head xs; l = length xs
        in case (d, l) of
          (3, _) -> 1
          -- Cheat and calculate them manually
          (1, 1) -> 1
          (1, 2) -> 2
          (1, 3) -> 4
          (1, 4) -> 7
          ) $ group diffs

  putStrLn $ show test
