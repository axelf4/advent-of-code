module Main where

import Data.Maybe (mapMaybe)
import Control.Monad (msum)
import qualified Data.Vector.Unboxed as V
import Data.Function (fix)

data Map = Map Int (V.Vector Char)
  deriving (Eq)

parseMap :: String -> Map  
parseMap s = let
  rows = lines s
  width = length $ head rows
  vec = V.fromList $ concat rows
  in Map width vec

(!) :: Map -> (Int, Int) -> Maybe Char  
(Map width vec) ! (x, y) =
  if 0 <= x && x < width && 0 <= y
  then vec V.!? (x + width * y)
  else Nothing

deltas :: [(Int, Int)]
deltas = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

translate :: (Int, Int) -> (Int, Int) -> (Int, Int)
translate (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scale :: Int -> (Int, Int) -> (Int, Int)
scale k (x, y) = (k * x, k * y)

adjacent :: (Int, Int) -> Map -> [Char]
adjacent c m = mapMaybe ((m!) . (translate c)) deltas

stepSeat1 :: Map -> (Int, Int) -> Char
stepSeat1 m c =
  let Just cur = m ! c
      numAdjacentOccupied = length $ filter (=='#') $ adjacent c m
  in case cur of
    'L' | numAdjacentOccupied == 0 -> '#'
    '#' | numAdjacentOccupied >= 4 -> 'L'
    x -> x
  
longAdjacent :: (Int, Int) -> Map -> [Char]
longAdjacent c m = let
  cast delta = let Just res = msum $ (\k -> case m ! (translate c $ scale k delta) of
                             Just 'L' -> Just (Just 'L')
                             Just '#' -> Just (Just '#')
                             Just '.' -> Nothing
                             Nothing -> Just Nothing -- Did not see anything in this dir
                               ) <$> [1..]
               in res
  in mapMaybe cast deltas

stepSeat2 :: Map -> (Int, Int) -> Char
stepSeat2 m c =
  let Just cur = m ! c
      numAdjacentOccupied = length $ filter (=='#') $ longAdjacent c m
  in case cur of
    'L' | numAdjacentOccupied == 0 -> '#'
    '#' | numAdjacentOccupied >= 5 -> 'L'
    x -> x
  
step :: (Map -> (Int, Int) -> Char) -> Map -> Map
step stepSeat m@(Map width vec) = Map width (V.imap f vec)
  where
    i2c i = (i `rem` width, i `div` width)
    f i _ = stepSeat m (i2c i)

nrOccupiedSeats (Map _ vec)
  = V.foldl' (\acc x -> acc + if x == '#' then 1 else 0) 0 vec

fixpoint step = fix (\next m -> if step m == m then m else next $ step m)

main :: IO ()
main = do
  map <- parseMap <$> readFile "input"

  let fixpoint1 = fixpoint (step stepSeat1) map
  putStrLn $ show (nrOccupiedSeats fixpoint1)

  let fixpoint2 = fixpoint (step stepSeat2) map
  putStrLn $ show (nrOccupiedSeats fixpoint2)
