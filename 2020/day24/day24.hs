import Data.List (foldl')
import Text.Parsec (try, many1, choice, sepEndBy1, char, string, endOfLine, eof)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Functor.Identity (Identity (runIdentity))

data Direction = E | SE | SW | W | NW | NE

allDirs = [E, SE, SW, W, NW, NE]

data Axial = Axial Int Int
  deriving (Eq, Ord, Show)

instance Num Axial where
  Axial a b + Axial c d = Axial (a+c) (b+d)
  Axial a b * Axial c d = Axial (a*c) (b*d)
  Axial a b - Axial c d = Axial (a-c) (b-d)
  abs (Axial a b) = Axial (abs a) (abs b) 
  signum (Axial a b) = Axial (signum a) (signum b) 
  fromInteger i = Axial (fromInteger i) (fromInteger i)

instance Semigroup Axial where
  (<>) = (+)

instance Monoid Axial where
  mempty = origin

origin = Axial 0 0

dirToAxial dir = case dir of
  E -> Axial (-1) 0
  SE -> Axial (-1) (-1)
  SW -> Axial 0 (-1)
  W -> Axial 1 0
  NW -> Axial 1 1
  NE -> Axial 0 1

adjacent :: Axial -> [Axial]
adjacent c = (c +) <$> dirToAxial <$> allDirs

parseInput f = either (error . show) id <$> parseFromFile (parser <* eof) f
  where
    parser = line `sepEndBy1` endOfLine
    line = many1 $ choice [ char 'e' *> pure E
                          , try (string "se") *> pure SE
                          , try (string "sw") *> pure SW
                          , char 'w' *> pure W
                          , try (string "ne") *> pure NE
                          , try (string "nw") *> pure NW
                          ]

step :: Set Axial -> Set Axial
step blacks = (blacks `Set.difference` newWhites) `Set.union` newBlacks
  where
    whites = (Set.fromList $ Set.toList blacks >>= adjacent) `Set.difference` blacks
    numAdjBlacks = length . filter (`Set.member` blacks) . adjacent
    newWhites = Set.filter ((\n -> n == 0 || n > 2) . numAdjBlacks) blacks
    newBlacks = Set.filter ((== 2) . numAdjBlacks) whites

alter f k = runIdentity . Set.alterF (pure . f) k

main :: IO ()
main = do
  input <- parseInput "input"
  let blacks = foldl'
        (\blacks dirs -> alter not (mconcat (dirToAxial <$> dirs)) blacks)
        Set.empty
        input
      after100Days = iterate step blacks !! 100
  putStrLn $ show (Set.size blacks)
  putStrLn $ show (Set.size after100Days)
