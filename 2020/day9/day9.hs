import Data.List (inits, tails)
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Applicative (liftA2)

windows :: Int -> [a] -> [[a]]
windows n = 
  (takeWhile (\xs -> length xs >= n)) . (map $ take n) . tails

isBad :: ([Int], Int) -> Bool
isBad (prev, elem)
  = null [(x, y)
         | x <- prev, y <- prev, x /= y
         , x + y == elem]

main = do
  numbers <- (\s -> map read $ lines s) <$> readFile "input"

  -- Part One
  let n = 25

  let cases = map (\window -> let (xs, [elem]) = splitAt n window
                                in (xs, elem))
        $ windows (n+1) numbers

  let bad = snd $ head $ filter isBad $ cases

  putStrLn $ show bad

  -- Part Two
  let sumSet =
        let shrinkToAcceptable
              = listToMaybe . (filter ((>=2) . length))
              . (filter ((==bad) . sum)) . tails
        in head $ mapMaybe shrinkToAcceptable $ inits numbers

  putStrLn $ show (liftA2 (+) minimum maximum $ sumSet)
  
  return 0
