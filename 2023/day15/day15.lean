-- AOC 2023 day 15 in Lean

def sum := List.foldr (·+·) 0

def HASH (s: String) : Nat := String.foldl f 0 s
  where f := fun acc x => (acc + Char.toNat x) * 17 % 256

def step (acc : Array (Array (String × Nat))) (s : String)
  : Array (Array (String × Nat)) :=
  let p := s.prev s.endPos
  let last := s.back
  let (label, focalLength) := if last == '-'
    then (s.extract 0 p, none)
    else (s.extract 0 (s.prev p), some (last.toNat - '0'.toNat))
  acc.modify (HASH label) fun lenses =>
    let i := lenses.findIdx? fun (x, _) => x == label
    if let some focalLength := focalLength then
      (if let some i := i then lenses.set! i else lenses.push)
        (label, focalLength)
    else
      if let some i := i then lenses.eraseIdx i else lenses

def totalFocusingPower (boxes : Array (Array (String × Nat))) := Prod.snd (boxes.foldl
  (fun (i, sum) lenses => (i + 1, sum + i * Prod.snd (lenses.foldl
    (fun (j, sum2) (_, focalLength) => (j + 1, sum2 + j * focalLength))
    (1, 0))))
  (1, 0))

def main : IO Unit := do
  let line ← String.trimRight
    <$> IO.FS.withFile "input" IO.FS.Mode.read IO.FS.Handle.getLine
  let steps := String.splitOn line ","
  let part1 := sum (List.map HASH steps)
  let boxes0 := Array.mkArray 256 Array.mkArray0
  let part2 := totalFocusingPower (steps.foldl step boxes0)
  IO.println s!"Part 1: {part1}\nPart 2: {part2}"
