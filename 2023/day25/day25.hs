{-# LANGUAGE BlockArguments, ScopedTypeVariables #-}

import Control.Monad (liftM, forM, filterM)
import Control.Monad.ST (ST, runST)
import Data.Function (fix)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.ST (STArray, STUArray, newArray, newListArray, newGenArray, readArray, writeArray)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Containers.ListUtils (nubOrd)
import Text.Parsec (ParseError, parse, many1, sepBy1, sepEndBy, char, letter, string, eof)
import System.Random (StdGen, randomR, initStdGen)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

shuffle' :: forall a. [a] -> StdGen -> ([a], StdGen)
shuffle' xs gen = runST do
    g <- newSTRef gen
    let randomRST lohi = do
          (a, s') <- liftM (randomR lohi) (readSTRef g)
          writeSTRef g s'
          pure a
    ar <- newListArray (1, n) xs :: ST s (STArray s Int a)
    xs' <- forM [1..n] $ \i -> do
      j <- randomRST (i, n)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      pure vj
    gen' <- readSTRef g
    pure (xs', gen')
  where
    n = length xs

edges :: Map a [a] -> [(a, a)]
edges x = Map.toList x >>= \(k, v) -> (k, ) <$> v

type Node = Int

toIntEdges :: [(String, String)] -> [(Int, Int)]
toIntEdges = snd . List.mapAccumL f Map.empty
  where
    g s x = case Map.lookup x s of
      Nothing -> let y = Map.size s in (Map.insert x y s, y)
      Just y -> (s, y)
    f s (a, b) = let
        (s', a') = g s a
        (s'', b') = g s' b
      in (s'', (a', b'))

bidirectionalized :: Map String [String] -> Map Node [Node]
bidirectionalized x
  = Map.fromListWith (++) (mapSnd List.singleton <$> nubOrd (es <> es'))
  where
    flipEdge (x, y) = (y, x)
    es = toIntEdges $ edges x
    es' = flipEdge <$> es

parseInput :: String -> Either ParseError (Map Node [Node])
parseInput input = bidirectionalized . Map.fromList <$> parse lines "" input
  where
    component = many1 letter
    line = do
      start <- component
      string ": "
      connections <- component `sepBy1` (char ' ')
      pure (start, connections)
    lines = (line `sepEndBy` (char '\n')) <* eof

karger :: Map Node [Node] -> StdGen -> (Maybe Int, StdGen)
karger map gen = (, gen') $ runST do
  -- Uses Karger's algorithm to probabilistically contract non-minimum
  -- cut edges, while maintaining merged node sizes with Union-Find.

  remap <- newGenArray (0, n - 1) (pure . id) :: ST s (STUArray s Int Int)
  sizes <- newArray (0, n - 1) 1 :: ST s (STUArray s Int Int)
  let
    find x = readArray remap x >>= \x' ->
      if x' == x then pure x
      else do x'' <- readArray remap x'
              writeArray remap x x''
              find x''

    go n ((a, b) : es)
      | n <= 2 = do
          es' <- filterM (\(a, b) -> (/=) <$> find a <*> find b) allEdges
          if length es' == 2 * 3 then do
            let ((a, b) : _) = es'
            size1 <- readArray sizes =<< find a
            size2 <- readArray sizes =<< find b
            pure $ Just (size1 * size2)
          else pure Nothing
      | otherwise = do
          x <- find a
          y <- find b
          size1 <- readArray sizes x
          size2 <- readArray sizes y
          if x == y then go n es else do
            let (x', y') = if size1 < size2 then (y, x) else (x, y)
            writeArray remap y' x'
            writeArray sizes x' (size1 + size2)
            go (n - 1) es
  go n es
  where
    n = Map.size map
    allEdges = edges map
    (es, gen') = shuffle' allEdges gen

main = do
  gen <- initStdGen
  input <- either (error . show) id . parseInput <$> readFile "input"
  let answer = flip fix gen \rec gen -> case karger input gen of
        (Nothing, gen') -> rec gen'
        (Just x, _) -> x
  putStrLn $ show answer
