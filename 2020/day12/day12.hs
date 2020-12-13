import Data.List (foldl')
import Data.Function (on)
import Debug.Trace

step1 :: (Int, Int, Int) -> String -> (Int, Int, Int)
step1 (x, y, dir) instruction =
  let
    action:value = instruction
    v = read value
  in case action of
    'N' -> (x, y + v, dir)
    'S' -> (x, y - v, dir)
    'E' -> (x + v, y, dir)
    'W' -> (x - v, y, dir)
    'L' -> (x, y, dir - v)
    'R' -> (x, y, dir + v)
    'F' -> let (dx, dy) = delta in (x + v * dx, y + v * dy, dir)
  where
    delta = case dir `mod` 360 of
      0 -> (1, 0)
      90 -> (0, -1)
      180 -> (-1, 0)
      270 -> (0, 1)

distance = (+) `on` abs

step2 :: (Int, Int, Int, Int) -> String -> (Int, Int, Int, Int)
step2 (x, y, wx, wy) instruction =
  let
    action:value = instruction
    v = read value
  in case action of
    'N' -> (x, y, wx, wy + v)
    'S' -> (x, y, wx, wy - v)
    'E' -> (x, y, wx + v, wy)
    'W' -> (x, y, wx - v, wy)
    'L' -> let (nwx, nwy) = rotateLeft v (wx, wy) in (x, y, nwx, nwy)
    'R' -> let (nwx, nwy) = rotateRight v (wx, wy) in (x, y, nwx, nwy)
    'F' -> (x + v * wx, y + v * wy, wx, wy)
  where
    rotateLeft angle (wx, wy) = case angle `mod` 360 of
      90 -> (-wy, wx)
      180 -> (-wx, -wy)
      270 -> (wy, -wx)
    rotateRight angle = rotateLeft (-angle)

main = do
  instructions <- lines <$> readFile "input"

  let (x1, y1, _) = foldl' step1 (0, 0, 0) instructions
  putStrLn $ show (distance x1 y1)

  let (x2, y2, _, _) = foldl' step2 (0, 0, 10, 1) instructions
  putStrLn $ show (distance x2 y2)
