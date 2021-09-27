import Data.Int (Int64)
import Text.Parsec (ParseError, parse, char, string, (<|>), many1, count, digit, try)
import Data.List (foldl')
import Data.Bits
import Control.Monad (void)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Debug.Trace

data Mask = Mask { mask1s :: Int64, maskXs :: Int64 }
  deriving (Show)

data Action
  = SetMask Mask
  | WriteMem { address :: Int, value :: Int64 }
  deriving (Show)

parseAction :: String -> Either ParseError Action
parseAction = parse parser ""
  where
    parseMask s =
      let (ones, xs) = foldl'
            (\(n, m) c -> ((shiftL n 1) .|. case c of '1' -> 1; _ -> 0,
                          (shiftL m 1) .|. case c of 'X' -> 1; _ -> 0))
            (0, 0) s
      in Mask { mask1s = ones, maskXs = xs }
    updateMask = do
      void $ try (string "mask = ")
      mask <- parseMask <$> count 36 (char 'X' <|> char '0' <|> char '1')
      return $ SetMask mask
    writeMem = do
      void $ try (string "mem[")
      addr <- read <$> many1 digit
      void $ string "] = "
      val <- read <$> many1 digit
      return WriteMem { address = addr, value = val }
    parser = updateMask <|> writeMem

bits :: FiniteBits b => b -> [Int]
bits b | b == zeroBits = []
bits b | otherwise = let i = countTrailingZeros b in i : bits (clearBit b i)

data State = State Mask (IntMap Int64)

step :: State -> Action -> State
step (State mask@(Mask { mask1s = mask1s, maskXs = maskXs }) mem) action = case action of
  SetMask newMask -> State newMask mem
  WriteMem { address = address , value = value } -> writeMem2 address value
  where
    writeMem1 address value = let
        v = ((value .&. maskXs) .|. mask1s)
        newMem = IntMap.insert address v mem
      in State mask newMem

    writeMem2 address value = let
        floatingBits = bits maskXs
        a = (fromIntegral address) .|. mask1s
        addresses = foldr (\f acc -> acc >>= \a -> [setBit a f, clearBit a f]) [a] floatingBits
        mem' = foldl' (\acc a -> IntMap.insert (fromIntegral a) value acc) mem addresses
      in State mask mem'

main = do
  input <- readFile "input"
  let actions = case sequence $ parseAction <$> lines input of
        Left e -> error $ show e
        Right x -> x

  let initialMask = Mask { mask1s = 0, maskXs = 0 }
  let initialState = State initialMask IntMap.empty

  let State _ mem = foldl' step initialState actions
  let sum = foldl' (+) 0 mem
  putStrLn $ show sum
