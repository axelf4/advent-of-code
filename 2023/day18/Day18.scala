// AOC 2023 day 18 in Scala

import scala.io.Source

enum Direction:
  case R, D, L, U

def displace(dir: Direction, p: (Int, Int), n: Int) =
  val (x, y) = p
  dir match
    case Direction.R => (x + n, y)
    case Direction.D => (x, y + n)
    case Direction.L => (x - n, y)
    case Direction.U => (x, y - n)

@main def main: Unit =
  val lines = Source.fromFile("input").getLines
  val input = lines.map(line =>
    val parts = line.split(' ')

    // Part 1
    // val dir = parts(0) match
    //   case "R" => Direction.R
    //   case "D" => Direction.D
    //   case "L" => Direction.L
    //   case "U" => Direction.U
    // val n = parts(1).toInt

    // Part 2
    val n = Integer.parseUnsignedInt(parts(2).substring(2, 7), 16)
    val dir = parts(2)(7) match
      case '0' => Direction.R
      case '1' => Direction.D
      case '2' => Direction.L
      case '3' => Direction.U

    (dir, n)
  ).toArray

  val b = input.map(_._2).sum
  val points = input.scanLeft((0, 0))((p, x) => displace(x._1, p, x._2))
  val area = points.sliding(2, 1).map(xs =>
    val (x1, y1) = xs(0)
    val (x2, y2) = xs(1)
    (y1 + y2).toLong * (x1 - x2).toLong // Shoelace formula
  ).sum.abs / 2 + b / 2 + 1 // Pick's theorem
  println(s"Area: $area")
