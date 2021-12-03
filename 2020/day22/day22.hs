import Control.Monad (void)
import Text.Parsec (string, eof, many1, sepEndBy1)
import Text.Parsec.Char (endOfLine, digit)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))

parseInput :: String -> IO ([Int], [Int])
parseInput f = either (error . show) id <$> parseFromFile (parser <* eof) f
  where
    number = read <$> many1 digit
    parser = do
      void $ string "Player 1:" <* endOfLine
      cards1 <- number `sepEndBy1` endOfLine
      void $ endOfLine
      void $ string "Player 2:" <* endOfLine
      cards2 <- number `sepEndBy1` endOfLine
      pure (cards1, cards2)

data Player = Player1 | Player2
  deriving (Show)

calcScore = sum . Seq.mapWithIndex (\i c -> c * (1 + i)) . Seq.reverse

playCombat :: (Seq Int, Seq Int) -> (Player, Int)
playCombat (deck1, deck2) = case (deck1, deck2) of
  (deck1, Empty) -> (Player1, calcScore deck1)
  (Empty, deck2) -> (Player2, calcScore deck2)
  (c1 :<| d1, c2 :<| d2)
    | c1 > c2 -> playCombat (d1 :|> c1 :|> c2, d2)
    | c1 < c2 -> playCombat (d1, d2 :|> c2 :|> c1)

playRecCombat :: (Seq Int, Seq Int) -> (Player, Int)
playRecCombat = go Set.empty
  where
    go seen config@(deck1, deck2)
      | config `Set.member` seen = (Player1, calcScore deck1)
      | otherwise =
        case config of
          (deck1, Empty) -> (Player1, calcScore deck1)
          (Empty, deck2) -> (Player2, calcScore deck2)
          (c1 :<| d1, c2 :<| d2) -> let
            winner = if Seq.length d1 >= c1 && Seq.length d2 >= c2
                then fst $ playRecCombat (Seq.take c1 d1, Seq.take c2 d2)
                else if c1 > c2 then Player1 else Player2
            nextConfig = case winner of
              Player1 -> (d1 :|> c1 :|> c2, d2)
              Player2 -> (d1, d2 :|> c2 :|> c1)
            in go (Set.insert config seen) nextConfig

main :: IO ()
main = do
  (deck1, deck2) <- parseInput "input"
  let cfg = (Seq.fromList deck1, Seq.fromList deck2)
  putStrLn $ show (playCombat cfg)
  putStrLn $ show (playRecCombat cfg)
