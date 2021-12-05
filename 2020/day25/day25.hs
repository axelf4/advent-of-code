import Data.Maybe (fromJust)
import Data.List (elemIndex)

step :: Int -> Int -> Int
step subjectNr i = (i * subjectNr) `rem` 20201227

findLoopSize :: Int -> Int -> Int
findLoopSize subjectNr pubKey = fromJust $ elemIndex pubKey $ iterate (step subjectNr) 1 

main = do
  let
    cardPubKey = 5099500
    doorPubKey = 7648211

    cardLoopSize = findLoopSize 7 cardPubKey
    doorLoopSize = findLoopSize 7 doorPubKey

    encryptionKey1 = iterate (step doorPubKey) 1 !! cardLoopSize
    encryptionKey2 = iterate (step cardPubKey) 1 !! doorLoopSize

  putStrLn $ "Card loop size: " <> show cardLoopSize
  putStrLn $ "Door loop size: " <> show doorLoopSize

  putStrLn $ show encryptionKey1
  putStrLn $ show encryptionKey2
