module Main where

import Control.Applicative (liftA2)
import Control.Monad (when, forever)
import Data.Functor (($>))
import Control.Monad.ST
import Data.STRef
import Text.Parsec
import qualified Data.Vector as V
import qualified Data.IntSet as IS

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except

data Op = Acc | Jmp | Nop

data Instruction = Instruction Op Int

singleton x = [x]

parseInput :: String -> Either ParseError [Instruction]
parseInput = parse instructions ""
  where
    sign = (char '+' $> 1) <|> (char '-' $> -1)
    number = liftA2 (*) sign (read <$> many1 digit)
    op = ((string "acc" $> Acc)
         <|> (string "jmp" $> Jmp)
         <|> (string "nop" $> Nop))
         <* spaces
    instruction = liftA2 Instruction op number
    instructions = instruction `sepEndBy` (char '\n') <* eof

data TerminationStatus = DidTerminate | DidNotTerminate
  deriving (Show, Eq)

vmST :: Int -> V.Vector Instruction -> (TerminationStatus, Int)
vmST toChange ins = runST $ do
  ip <- newSTRef 0
  acc <- newSTRef 0

  executed <- newSTRef IS.empty

  e <- runExceptT $ forever $ do
    curIp <- lift $ readSTRef ip
    curAcc <- lift $ readSTRef acc

    -- If attempting to run instruction below last one: Terminate
    when (curIp >= V.length ins) $ throwE (DidTerminate, curAcc)
  
    isMember <- lift $ (curIp `IS.member`) <$> readSTRef executed
    when isMember $ throwE (DidNotTerminate, curAcc)
    
    lift $ do
      modifySTRef executed (IS.insert curIp)
      
      instruction <- (\instruction -> case instruction of
                         Instruction Jmp n | curIp == toChange -> Instruction Nop n
                         Instruction Nop n | curIp == toChange -> Instruction Jmp n
                         x -> x)
        <$> (ins V.!) <$> readSTRef ip
  
      shouldIncrIP <- case instruction of
        Instruction Acc n -> modifySTRef' acc (+n) >> (pure True)
        Instruction Jmp n -> modifySTRef' ip (+n) >> (pure False)
        Instruction Nop _ -> pure True
  
      if shouldIncrIP then modifySTRef' ip (+1) else pure ()

    return ()
  return (case e of
            Left acc -> acc
            _ -> error "Unreachable")

main = do
  input <- readFile "input"
  let instructions = V.fromList (case parseInput input of
        Left e -> error $ show e
        Right x -> x)

  putStrLn $ show (vmST (-1) instructions)

  -- Part 2
  let acc = head [acc
                       | i <- [0..V.length instructions]
                       , let (status, acc) = vmST i instructions
                       , status == DidTerminate]
  putStrLn $ show acc
