import qualified Data.Map as Map
import Data.Maybe (isJust)

data Vector a =
  Vector a a a
  deriving (Show)

getX (Vector x _ _) = x

getY (Vector _ y _) = y

getZ (Vector _ _ z) = z

type IVec = Vector Int

zero = Vector 0 0 0

vmap f (Vector a b c) = Vector (f a) (f b) (f c)

instance Foldable Vector where
  foldMap f (Vector a b c) = f a `mappend` f b `mappend` f c

instance (Num a) => Num (Vector a) where
  Vector a b c + Vector x y z = Vector (a + x) (b + y) (c + z)
  Vector a b c * Vector x y z = Vector (a * x) (b * y) (c * z)
  negate (Vector a b c) = Vector (-a) (-b) (-c)
  abs (Vector a b c) = Vector (abs a) (abs b) (abs c)
  signum (Vector a b c) = Vector (signum a) (signum b) (signum c)
  fromInteger i = Vector (fromInteger i) (fromInteger i) (fromInteger i)

data Body =
  Body
    { pos :: IVec
    , vel :: IVec
    }
  deriving (Show)

applyGravity :: [Body] -> [Body]
applyGravity bodies =
  map
    (\Body {pos = p, vel = v} ->
       let nv = v + sum (map (\x -> signum (pos x - p)) bodies)
        in Body {pos = p, vel = nv})
    bodies

applyVelocity :: [Body] -> [Body]
applyVelocity = map (\Body {pos = p, vel = v} -> Body {pos = p + v, vel = v})

step :: [Body] -> [Body]
step = applyVelocity . applyGravity

energy :: [Body] -> Int
energy =
  sum .
  map
    (\Body {pos = p, vel = v} ->
       let potential = sum (abs p)
           kinetic = sum (abs v)
        in potential * kinetic)

findCycle :: (IVec -> Int) -> [Body] -> Int
findCycle memberFunc bodies =
  let getSingle = map (\Body {pos = p, vel = v} -> (memberFunc p, memberFunc v))
      go map n bodies =
        let entry = getSingle bodies
            (old, map') = Map.insertLookupWithKey (\_ a _ -> a) entry n map
         in if isJust old
              then n
              else go map' (n + 1) (step bodies)
   in go Map.empty 0 bodies

printBodies :: [Body] -> IO [()]
printBodies = sequence . (map (putStrLn . show))

main = do
  let bodies =
        [ Body {pos = Vector (-7) (-8) 9, vel = zero}
        , Body {pos = Vector (-12) (-3) (-4), vel = zero}
        , Body {pos = Vector 6 (-17) (-9), vel = zero}
        , Body {pos = Vector 4 (-10) (-6), vel = zero}
        ]
  putStrLn "After 0 steps:"
  printBodies bodies
  let newBodies = (iterate step bodies) !! 1000
  putStrLn "After 1000 steps:"
  printBodies newBodies
  putStrLn ("Energy in system: " ++ (show $ energy newBodies))
  let cycle =
        foldr
          (\a b -> a * b `div` (gcd a b))
          1
          (map (\memberFunc -> findCycle memberFunc bodies) [getX, getY, getZ])
  putStrLn ("Steps til cycle: " ++ show cycle)
