-- AOC 2023 day 9 in Futhark

def extrapolate (xs: []i32): (i32, i32) =
  let (n, _, firsts, lasts) =
    loop (i, arr, firsts, lasts) =
      (0, xs, replicate (length xs) (head xs), replicate (length xs) (last xs))
    while any (!= 0) arr do
    let i' = i + 1
    let diff = map2 (-) (tail arr) (init arr)
    in (i', diff, firsts with [i'] = head diff, lasts with [i'] = last diff)
  in (foldr (-) 0 (take n firsts), reduce (+) 0 (take n lasts))

def main (xs: [][]i32): (i32, i32) =
  let (beginnings, ends) = map extrapolate xs |> unzip
  in (reduce (+) 0 beginnings, reduce (+) 0 ends)
