import Control.Monad (void, guard, foldM)
import qualified Data.List as List
import Data.List (find)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Text.Parsec (ParseError, (<|>), (<?>), char, string, digit, eof, many, many1, count, sepEndBy1)
import Text.Parsec.Char (oneOf, anyChar, endOfLine, letter)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Vector.Unboxed as VU

n = 10 -- Tiles are 10x10 pixels

data Tile = Tile { tileId :: Int, pixels :: VU.Vector Bool }

parseInput :: String -> IO [Tile]
parseInput f = either (error . show) id <$> parseFromFile parser f
  where
    parser = tile `sepEndBy1` many endOfLine
    tile = do
      id <- read <$> (string "Tile " *> many1 digit <* char ':' <* endOfLine)
      chars <- count n $ count n (oneOf ['.', '#']) <* endOfLine
      let pixels = VU.fromListN (n * n) $ do
            line <- chars
            ch <- line
            pure $ case ch of
              '.' -> False
              '#' -> True
      pure $ Tile { tileId = id, pixels = pixels }

type Coord = (Int, Int)
type IndexFn = Coord -> Int

orientations :: Int -> [IndexFn]
orientations n = [f (r orig)
                 | r <- [id, rotate, rotate . rotate, rotate . rotate . rotate]
                 , f <- [id, flip]
                 ]
  where
    orig (x, y) = x + n * y
    rotate f (x, y) = f (y, n - 1 - x)
    flip f (x, y) = f (n - 1 - x, y)

type OTile = (Tile, IndexFn) -- An oriented tile

selections :: [a] -> [(a, [a])]
selections [] = []
selections (x : xs) = (x, xs) : ((\(y, ys) -> (y, x : ys)) <$> selections xs)

arrange :: Int -> [Tile] -> Map Coord OTile
arrange m tiles = arrangement
  where
    coords = [(x, y) | y <- [0..m - 1], x <- [0..m - 1]]
    (arrangement, []) = head $ foldM arrangeTile (Map.empty, tiles) coords

    arrangeTile :: (Map Coord OTile, [Tile]) -> Coord ->  [(Map Coord OTile, [Tile])]
    arrangeTile (prev, tiles) coord@(x, y) = do
      (tile, remaining) <- selections tiles
      f <- orientations n

      guard $ fromMaybe True $ do
        (below, g) <- prev Map.!? (x, y - 1)
        pure $ all (\i -> pixels tile VU.! f (i, 0) == pixels below VU.! g (i, n - 1)) [0..n - 1]
      guard $ fromMaybe True $ do
        (left, g) <- prev Map.!? (x - 1, y)
        pure $ all (\i -> pixels tile VU.! f (0, i) == pixels left VU.! g (n - 1, i)) [0..n - 1]

      pure (Map.insert coord (tile, f) prev, remaining)

main :: IO ()
main = do
  tiles <- parseInput "input"
  let
    m = round $ sqrt $ fromIntegral (length tiles)
    arrangement = arrange m tiles
    corners = (arrangement Map.!) <$> [(0, 0), (m-1, 0), (m-1, m-1), (0, m-1)]

    realSize = m * (n - 2)

    f :: Int -> Bool
    f i = let
      (y, x) = i `quotRem` realSize
      (cx, x') = x `quotRem` (n - 2)
      (cy, y') = y `quotRem` (n - 2)
      (Tile { pixels = pixels }, g) = arrangement Map.! (cx, cy)
      in pixels VU.! g (1 + x', 1 + y')

    img = [ [ if f (x + realSize * y) then '#' else '.'
            | x <- [0..realSize-1]
            ]
          | y <- [0..realSize-1]
          ]

    monsterImg = [ "                  # "
                 , "#    ##    ##    ###"
                 , " #  #  #  #  #  #   "
                 ]
    monster = map (== '#') <$> monsterImg

    countMonsters f = length $ filter id $ do
      y <- [0..realSize - length monster]
      x <- [0..realSize - length (head monster)]
      pure $ and $ do
        (y', row) <- zip [0..] monster
        (x', p) <- zip [0..] row
        pure $ not p || f (x + x', y + y')
    monsterCount = fromJust $ find (> 0)
      $ map (countMonsters . (f .))
      $ orientations realSize
    countHashes = length . filter (== '#') . concat

  putStrLn $ show $ product $ (\(Tile { tileId = id }, _) -> id) <$> corners
  putStrLn (unlines img)
  putStrLn $ show $ countHashes img - monsterCount * countHashes monsterImg
