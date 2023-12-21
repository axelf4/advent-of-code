module Main (main) where

import Prelude
import Data.Array as Array
import Data.Array (concat, mapMaybe)
import Data.Foldable (all, elem, foldl, length)
import Data.Function (applyN)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..), codePointFromChar, trim)
import Data.String as String
import Data.Traversable (Accum, mapAccumL)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Partial.Unsafe (unsafePartial)

data Module
  = FlipFlop Boolean
  | Conjunction (Map String Boolean)
  | Broadcast

type State = Map String (Tuple Module (Array String))

parse :: Partial => String -> State
parse s = patchConjunctions $ Map.fromFoldable $ parseLine <$> lines (trim s)
  where
    lines = String.split (Pattern "\n")
    parseLine l = let
      [a, b] = String.split (Pattern " -> ") l
      Tuple name mod = case String.uncons a of
        Just { head: x, tail: name }
          | x == codePointFromChar '%' -> Tuple name (FlipFlop false)
          | x == codePointFromChar '&' -> Tuple name (Conjunction Map.empty)
        _ -> Tuple a Broadcast
      dsts = String.split (Pattern ", ") b
      in Tuple name (Tuple mod dsts)
    patchConjunctions xs = mapWithIndex f xs
      where f = case _, _ of
              k, Tuple (Conjunction _) outputs -> let
                mem0 = false <$ Set.toMap (connectedInputs k xs)
                in Tuple (Conjunction mem0) outputs
              _, x -> x
    connectedInputs k = Map.keys <<< Map.filter \(Tuple _ outputs) -> elem k outputs

type Pulse = { from :: String, to :: String, high :: Boolean }

processPulses :: State -> Array Pulse -> Tuple State (Array Pulse)
processPulses = (merge <<< _) <<< mapAccumL f
  where
    merge { accum: state, value: values } = Tuple state (concat values)

    f :: State -> Pulse -> Accum State (Array Pulse)
    f state { from, to, high } = case Map.lookup to state of
      Just (Tuple mod outputs) -> let
        Tuple mod' newPulse = case mod of
          Broadcast -> Tuple Broadcast (Just high)
          x@(FlipFlop _) | high -> Tuple x Nothing
          FlipFlop b -> let b' = not b in Tuple (FlipFlop b') (Just b')
          Conjunction mem -> let mem' = Map.insert from high mem
                                 output = not $ all identity mem'
                             in Tuple (Conjunction mem') (Just output)
        in { accum: Map.insert to (Tuple mod' outputs) state
           , value: case newPulse of
             Nothing -> []
             Just b -> { from: to, to: _, high: b } <$> outputs
           }
      Nothing -> { accum: state, value: [] }

solve1 :: State -> Int
solve1 input = numLow * numHigh
  where
    f [] acc = acc
    f pulses { state, numLow, numHigh } = let
      h = foldl (\acc { high } -> acc + if high then 1 else 0) 0 pulses
      l = length pulses - h
      Tuple state' pulses' = processPulses state pulses
      in f pulses' { state: state', numLow: numLow + l, numHigh: numHigh + h }

    initial = [{ from: "button", to: "broadcaster", high: false }]
    { numLow, numHigh } = applyN (f initial) 1000 { state: input, numLow: 0, numHigh: 0 }

toDot :: State -> String
toDot input = "strict digraph {\n" <> unlines stmts <> "\n}\n"
  where
    unlines = String.joinWith "\n"
    stmts = f <$> Map.toUnfoldableUnordered input
    f (Tuple name (Tuple mod outputs))
      = nodeAttrs <> "  " <> name <> " -> {" <> String.joinWith " " outputs <> "}"
      where
        nodeAttrs = case mod of
          Broadcast -> "  " <> name <> "[shape=doublecircle]"
          Conjunction _ -> "  " <> name <> "[shape=triangle]"
          _ -> ""

-- | Collects button press indices that generate high pulses to the final conjunction before rx.
solve2 :: State -> Array (Tuple String Int)
solve2 input = result
  where
    f [] {state, i, result} = {state, i: i + 1, result}
    f pulses { state, i, result } = let
      result' = mapMaybe
                (\{ from, high } ->
                  if high && elem from ["jz", "sv", "ft", "ng"]
                  then Just (Tuple from i) else Nothing)
                pulses
      Tuple state' pulses' = processPulses state pulses
      in f pulses' { state: state', i, result: result <> result' }

    initial = [{ from: "button", to: "broadcaster", high: false }]
    { result } = applyN (f initial) 100000 { state: input, i: 1, result: [] }

diff :: Array Int -> Array Int
diff xs = Array.zipWith (-) xs (Array.cons 0 xs)

main :: Effect Unit
main = do
  input <- unsafePartial $ parse <$> readTextFile UTF8 "input"

  writeTextFile UTF8 "input.dot" (toDot input)

  log $ show $ solve1 input -- Part 1

  -- Part 2:
  log $ show $ map diff $ Map.fromFoldableWith (flip (<>)) $ (map Array.singleton) <$> solve2 input
  -- See that they are periodic --> Apply LCM on those periods
