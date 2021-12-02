import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad (guard)

type Coord = (Int, Int, Int, Int)

neighbours :: Coord -> [Coord]
neighbours (x, y, z, w) = do
  x' <- [x - 1, x, x + 1]
  y' <- [y - 1, y, y + 1]
  z' <- [z - 1, z, z + 1]
  w' <- [w - 1, w, w + 1]
  guard $ x' /= x || y' /= y || z' /= z || w' /= w
  pure (x', y', z', w')

step :: Set Coord -> Set Coord
step s = Set.fromList $ filterActive active ++ filterInactive inactive
  where
    active = Set.toList s
    inactive = filter (flip Set.notMember s) $ active >>= neighbours

    activeNeighbourCount = length . filter (flip Set.member s) . neighbours

    filterActive = filter $ \c ->
      let cnt = activeNeighbourCount c in cnt == 2 || cnt == 3
    filterInactive = filter ((== 3) . activeNeighbourCount)

parseInput s = Set.fromList $ do
  (y, line) <- zip [0..] $ lines s
  (x, ch) <- zip [0..] line
  case ch of
    '#' -> pure (x, y, 0, 0)
    '.' -> mempty

main :: IO () 
main = do
  let input = "##.#....\n\
              \...#...#\n\
              \.#.#.##.\n\
              \..#.#...\n\
              \.###....\n\
              \.##.#...\n\
              \#.##..##\n\
              \#.####.."
  let final = iterate step (parseInput input) !! 6

  putStrLn $ show (Set.size final)
