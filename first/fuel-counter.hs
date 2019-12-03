import System.IO

fuelPerMass :: RealFrac a => a -> Int
fuelPerMass m = floor (m / 3) - 2

fuelForModule :: Float -> Int
fuelForModule m =
  let f = fuelPerMass m
   in if f > 0
        then f + (fuelForModule $ fromIntegral f)
        else 0

main = do
  input <- lines <$> readFile "input"
  let totFuel = sum $ (fuelForModule . read) <$> input
  putStrLn $ show totFuel
