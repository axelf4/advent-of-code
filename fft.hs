import Data.List (transpose, iterate')
import Data.Char (digitToInt)

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

repeatElems n = concat . transpose . replicate n

posPattern :: Int -> [Int]
posPattern p = drop 1 $ cycle $ repeatElems p $ basePattern

onesDigit :: Int -> Int
onesDigit = abs . (`rem` 10)

outputDigit :: Int -> [Int] -> Int
outputDigit p = onesDigit . sum . zipWith (*) (posPattern p)

phase :: [Int] -> [Int]
phase s = map (\p -> outputDigit p s) [1..length s]

fft :: Int -> [Int] -> [Int]
fft numPhases = (!! numPhases) . iterate' phase

digitsFromStr :: String -> [Int]
digitsFromStr = map digitToInt

inputSignal = "59773419794631560412886746550049210714854107066028081032096591759575145680294995770741204955183395640103527371801225795364363411455113236683168088750631442993123053909358252440339859092431844641600092736006758954422097244486920945182483159023820538645717611051770509314159895220529097322723261391627686997403783043710213655074108451646685558064317469095295303320622883691266307865809481566214524686422834824930414730886697237161697731339757655485312568793531202988525963494119232351266908405705634244498096660057021101738706453735025060225814133166491989584616948876879383198021336484629381888934600383957019607807995278899293254143523702000576897358"

main = do
  let signal = concat $ replicate 10000 (digitsFromStr inputSignal)
      msgOffset = read $ take 8 inputSignal :: Int
  putStrLn (show $ take 8 $ drop msgOffset $ fft 100 signal)
