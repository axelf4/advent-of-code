module Main where

import Control.Monad (foldM)
import qualified Data.Vector as V

data Square =
    O -- Open = .
  | T -- Tree = #
  deriving (Show)

-- A map has a width and a vector of squares
data Map = Map Int (V.Vector Square)

parseMap :: [String] -> Map
parseMap lines = let width = length $ head lines
                     parseSquare c = case c of '.' -> O
                                               '#' -> T
                                               _ -> error "Bad char"
                     squares = V.fromList $ parseSquare <$> (concat lines)
                 in Map width squares

at :: (Int, Int) -> Map -> Maybe Square
at (x, y) (Map width v) = 
  if y < 0 || x < 0 then error "Negative index"
  else
    let wx = x `rem` width
    in v V.!? (wx + width * y)

numTrees map (dx, dy)
  = case foldM
         (\numTrees (x, y) -> case at (x, y) map of
             Just O -> Right numTrees
             Just T -> Right (numTrees + 1)
             Nothing -> Left numTrees)
         0
         [(dx * k, dy * k) | k <- [0..]] of
      Left numTrees -> numTrees
      _ -> error "Unreachable"

main :: IO ()
main = do
  lines <- lines <$> readFile "input"

  let map = parseMap lines

  putStrLn $ "Number of trees: " ++ show (numTrees map (3,1))

  let result2 = product [numTrees map slope
                        | slope <- [(1,1), (3,1), (5,1), (7,1), (1,2)]]
  putStrLn $ "Product of result from the slopes: " ++ show result2
