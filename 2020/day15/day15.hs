-- Compile with optimizations or risk getting stack overflow exception.
import Data.List (foldl', length, last)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data State = State { lastOccurence :: IntMap Int, turn :: Int, next :: Int }
  deriving (Show)

initialState :: [Int] -> State
initialState startingNumbers =
  State { lastOccurence = foldl' (\acc (n, turn) -> IntMap.insert n turn acc)
                          IntMap.empty
                          $ zip startingNumbers [1..]
        , turn = length startingNumbers
        , next = 0
        }

step :: State -> State
step (State { lastOccurence = lastOccurence, turn = turn, next = n })
  = let turn' = turn + 1
        (lastSeen, lastOccurence')
          = IntMap.insertLookupWithKey (\_ nv _ -> nv) n turn' lastOccurence
    in case lastSeen of
         Just prevTurn -> State { lastOccurence = lastOccurence'
                                , turn = turn'
                                , next = turn' - prevTurn
                                }
         Nothing -> State { lastOccurence = lastOccurence'
                          , turn = turn'
                          , next = 0
                          }

main :: IO ()
main = do
  let n = 30000000 -- 2020

  let startingNumbers = [1, 0, 15, 2, 10, 13]
  let out = take (n - (length startingNumbers))
            $ map (\(State { next = n }) -> n)
            $ iterate step (initialState startingNumbers)


  putStrLn $ show $ last out
