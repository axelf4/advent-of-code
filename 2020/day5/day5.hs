import Data.List (foldl')
import qualified Data.IntSet as S

data Half = F | B

parseSeat :: String -> (Int, Int)
parseSeat s =
  let (rowSpec, colSpec) = splitAt 7 s
      f range = fst . (foldl' (\(a, b) x ->
                         let mid = (a + b) `div` 2
                         in case x of F -> (a, mid)
                                      B -> (mid, b))
                       range)
      row = f (0, 128) (map (\c -> case c of
                                'F' -> F
                                'B' -> B) rowSpec)
      col = f (0, 8) (map (\c -> case c of
                              'L' -> F
                              'R' -> B) colSpec)
  in (row, col)

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

yourSeat :: [Int] -> Int
yourSeat seatIds
  =
  -- Look for seat left of our seat, which will be in the set
  head [id+1 | id <- S.toList ids, (id+1) `S.notMember` ids, (id+2) `S.member` ids]
  where ids = S.fromList seatIds

main = do
  lines <- lines <$> readFile "input"
  let seatIds = map (seatId . parseSeat) lines

  putStrLn $ "Highest seat ID: " ++ show (maximum seatIds)
  putStrLn $ "Our seat ID: " ++ show (yourSeat seatIds)
