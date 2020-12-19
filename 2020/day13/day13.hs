import Text.Parsec
import Data.List (foldl', sortBy)
import Data.Function (on)
import Data.Maybe (catMaybes)

parseInput :: String -> Either ParseError (Int, [Maybe Int])
parseInput = parse parser ""
  where
    number = read <$> many1 digit
    bus = choice [Just <$> number, char 'x' >> return Nothing]
    parser = do
      t <- number <* newline
      buses <- bus `sepBy1` (char ',') <* spaces <* eof
      return (t, buses)

enumerate = zip [0..]

congruent n = (==) `on` (`mod` n)

main = do
  input <- readFile "input"
  let (t, buses) = case parseInput input of
        Left e -> error $ show e
        Right x -> x

  -- Part One
  let (earliestBus, waitTime)
        = head $ sortBy (compare `on` snd) $ map (\b -> (b, b - t `rem` b))
          $ catMaybes buses
  putStrLn $ show (earliestBus * waitTime)

  -- Part Two
  let (x, _) = foldl' (\(xp, np) (i, n) ->
          let arithProg = [xp + k * np | k <- [0..]]
              x = head $ filter (\x -> congruent n x (-i)) arithProg
          in (x, np * n)
         ) (1, 1)
        $ catMaybes $ map (\(i, b) -> case b of
                              Just n -> Just (i, n)
                              Nothing -> Nothing)
        $ enumerate buses
  putStrLn $ show x
