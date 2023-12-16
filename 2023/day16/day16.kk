import std/os/path
import std/os/file

fun (+)((x1, y1) : (int, int), (x2, y2) : (int, int)) : (int, int)
  (x1 + x2, y1 + y2)

alias pos = (int, int)

type dir { U; D; L; R }

fun delta(d : dir) : pos
  match d { U -> (0, -1); D -> (0, 1); L -> (-1, 0); R -> (1, 0) }

fun (==)(a : dir, b : dir) : bool
  match (a, b)
    (U, U) -> True
    (D, D) -> True
    (L, L) -> True
    (R, R) -> True
    _ -> False

fun next-dirs(^d : dir, ch : char) : exn list<dir>
  match ch
    '.' -> [d]
    '/' -> [match d { U -> R; D -> L; L -> D; R -> U }]
    '\\' -> [match d { U -> L; D -> R; L -> U; R -> D }]
    '|' -> match d { U -> [U]; D -> [D]; L -> [U, D]; R -> [U, D] }
    '-' -> match d { U -> [L, R]; D -> [L, R]; L -> [L]; R -> [R] }

value type energization
  Unenergized
  Energized(ds : list<dir>)

fun go(p : pos, d : dir, ^grid : vector<vector<char>>, states : ref<h, vector<energization>>)
  : <div, exn, read<h>, write<h>> ()
  val ch = if 0 <= p.snd && p.snd < grid.length && 0 <= p.fst && p.fst < grid[0].length
    then grid[p.snd][p.fst] else return ()
  val ds = next-dirs(d, ch)

  val i = p.fst + grid.length * p.snd
  val skip = states.modify fn(xs)
    val prev-state = xs[i]
    val new-state = match prev-state
      Unenergized -> Energized([d])
      Energized(ds') | ds'.any(fn(x) x == d) -> return True
      Energized(ds') -> Energized(Cons(d, ds'))
    xs[i] := new-state
    False
  if skip then return ()

  ds.foreach fn(d') go(p + d'.delta, d', grid, states)

fun num-energized(p : pos, d : dir, ^grid : vector<vector<char>>) : <div, exn> int
  val states = ref(vector(grid.length * grid[0].length, Unenergized))
  go(p, d, grid, states)
  (!states).vlist
    .filter(fn(x) match x { Unenergized -> False; _ -> True })
    .length

fun main()
  val input = read-text-file("input".path).trim-right.lines
  val grid = input.map(vector).vector
  val rows = grid.length
  val cols = grid[0].length

  println("Part 1: " ++ num-energized((0, 0), R, grid).show)

  val starts = list(0, cols - 1, fn(i) ((i, 0), D))
    ++ list(0, cols - 1, fn(i) ((i, rows - 1), U))
    ++ list(0, rows - 1, fn(i) ((0, i), R))
    ++ list(0, rows - 1, fn(i) ((cols - 1, i), L))
  val max = starts.map(fn((p, d)) num-energized(p, d, grid)).maximum(0)
  println("Part 2: " ++ max.show)
