import Control.Monad (liftM2)
import Data.List (minimumBy, transpose)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = (take n xs) : chunks n (drop n xs)

frequency :: Ord a => [a] -> Map.Map a Int
frequency xs =
  foldl (\m b -> Map.alter (Just . (1 +) . fromMaybe 0) b m) Map.empty xs

data Color
  = Black
  | White
  | Transparent

fromDigit :: Char -> Color
fromDigit p =
  case p of
    '0' -> Black
    '1' -> White
    '2' -> Transparent

instance Show Color where
  show c =
    case c of
      Black -> "0"
      White -> "1"
      Transparent -> "2"

instance Monoid Color where
  mempty = Transparent
  mappend a b =
    case (a, b) of
      (Transparent, _) -> b
      (_, Transparent) -> a
      _ -> b

width = 25

height = 6

pixelsPerImage = width * height

part1 layers =
  let minZero =
        minimumBy
          (comparing (fromMaybe 0 . Map.lookup '0'))
          (map frequency layers)
   in liftM2 (*) (Map.lookup '1' minZero) (Map.lookup '2' minZero)

main = do
  pixels <- readFile "input"
  let layers = chunks pixelsPerImage (concat $ lines pixels)
  -- putStrLn $ show (part1 layers)
  let layersColors = (map (map fromDigit) layers)
  let finalImage = map (mconcat . reverse) (transpose layersColors)
  let rows = chunks width finalImage
  sequence $ (putStrLn . show) <$> rows
