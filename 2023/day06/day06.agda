{-# OPTIONS --guardedness #-}

module day06 where

open import Category.Applicative using (RawApplicative)
open import Data.List.Categorical renaming (module TraversableA to ListTraversableA)
open import Data.Product using (Σ; _,_; _×_; uncurry)
open import Data.Bool using (Bool; true; false)
open import Data.Nat using (ℕ; zero; suc; _*_; _∸_; _≤ᵇ_)
open import Data.Nat.Show renaming (show to showℕ; readMaybe to readℕMaybe)
open import Data.Maybe as Maybe using (Maybe; just; nothing; _>>=_)
open import Data.List using (List; []; _∷_; map)
open import Data.Vec as Vec using (Vec; []; _∷_)
open import Data.String using (String; concat; lines; words)
open import IO using (run; putStrLn; readFiniteFile)

open ListTraversableA using (mapA)

maybeApplicative : ∀ {ℓ} -> RawApplicative {ℓ} Maybe
maybeApplicative = record
  { pure = just
  ; _⊛_  = λ { (just f) (just a) -> just (f a); _ _ -> nothing }
  }

zip : {A : Set} -> List A -> List A -> Maybe (Σ ℕ (Vec (A × A)))
zip [] [] = just (0 , [])
zip (x ∷ xs) (y ∷ ys) with zip xs ys
... | nothing = nothing
... | just (n , zs) = just (suc n , (x , y) ∷ zs)
zip [] (x ∷ ys) = nothing
zip (x ∷ xs) [] = nothing

parse : String -> Maybe (Σ ℕ (Vec (ℕ × ℕ)))
parse s with lines s
... | l1 ∷ (l2 ∷ []) = parseLine l1 >>= λ x -> parseLine l2 >>= λ y -> zip x y
  where
    parseLine : String -> Maybe (List ℕ)
    parseLine s with words s
    ... | _ ∷ xs = let -- ys = xs -- Part 1
                       ys = concat xs ∷ [] -- Part 2
      in mapA maybeApplicative (readℕMaybe 10) ys
    ... | [] = nothing
{-# CATCHALL #-}
... | _ = nothing

waysToWin : ℕ -> ℕ -> ℕ
waysToWin duration distance = go 0 duration
  where
    isWin : ℕ -> Bool
    isWin n = suc distance ≤ᵇ n * (duration ∸ n)

    go : ℕ -> ℕ -> ℕ
    go acc zero = acc
    go acc n@(suc m) with isWin n
    ... | true = go (suc acc) m
    ... | false = go acc m

product : {n : ℕ} -> Vec ℕ n -> ℕ
product = Vec.foldr _ _*_ 1

solve : String -> String
solve s with parse s
... | nothing = "Bad input"
... | just (_ , input) = showℕ (product (Vec.map (uncurry waysToWin) input))

main = run (readFiniteFile "input" IO.>>= λ s -> putStrLn (solve s))
