import Data.Maybe (fromJust, mapMaybe)
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))
import Control.Monad (replicateM_)
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MV

step :: Int -> Seq Int -> Seq Int
step max (c :<| x1 :<| x2 :<| x3 :<| xs) = let
  dst = fromJust $ List.find (`List.notElem` [x1, x2, x3]) ([c-1, c-2..1] <> [max, max-1..c+1])
  (init, _dst :<| tail) = Seq.breakl (== dst) xs
  in init <> (dst :<| x1 :<| x2 :<| x3 :<| tail) :|> c

partTwo :: Seq Int -> Int
partTwo labels@(first :<| _) = runST $ do
  cur <- newSTRef (first - 1)
  let max = 1000000
  -- Circular linked list. At index of each label is the index of the
  -- next node.
  v <- MV.generate
    max
    (\i -> if i < Seq.length labels
           then case Seq.breakl (== 1 + i) labels of
                  (_, _i' :<| next :<| _) -> next - 1
                  (_, _i' :<| Empty) -> Seq.length labels
           else if 1 + i < max then i + 1 else first - 1)

  replicateM_ 10000000 $ do
    c <- readSTRef cur
    x1 <- MV.read v c
    x2 <- MV.read v x1
    x3 <- MV.read v x2
    c' <- MV.read v x3
    MV.write v c c' -- Remove x1,x2,x3

    let dst = fromJust $ List.find
          (`List.notElem` [x1, x2, x3])
          ([c-1, c-2..0] <> [max-1, max-2..c+1])
    MV.exchange v dst x1 >>= MV.write v x3 -- Insert x1,x2,x3 after dst

    writeSTRef cur c'

  x1 <- MV.read v (1 - 1)
  x2 <- MV.read v x1
  pure $ (1 + x1) * (1 + x2)

main :: IO ()
main = do
  let labels = Seq.fromList [9, 6, 2, 7, 1, 3, 8, 5, 4]

  putStrLn $ show $ tail $ take (Seq.length labels) $ dropWhile (/= 1)
    $ cycle $ toList (iterate (step (maximum labels)) labels !! 100)
  
  putStrLn $ show (partTwo labels)
