use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fs;
use std::iter::repeat;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point(i32, i32);

impl Point {
    fn manhattan_dist(&self, p: Point) -> i32 {
        (p.0 - self.0).abs() + (p.1 - self.1).abs()
    }

    fn move_in_dir(&self, direction: Direction) -> Point {
        match direction {
            Direction::Up => Point(self.0, self.1 + 1),
            Direction::Right => Point(self.0 + 1, self.1),
            Direction::Down => Point(self.0, self.1 - 1),
            Direction::Left => Point(self.0 - 1, self.1),
        }
    }
}

const CENTRAL_PORT_LOC: Point = Point(0, 0);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl TryFrom<char> for Direction {
    type Error = &'static str;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'U' => Ok(Self::Up),
            'R' => Ok(Self::Right),
            'D' => Ok(Self::Down),
            'L' => Ok(Self::Left),
            _ => Err("Bad direction"),
        }
    }
}

fn parse_movement<'a>(s: &'a str) -> (Direction, usize) {
    let (dir, count) = s.split_at(1);
    (
        dir.chars().next().unwrap().try_into().unwrap(),
        count.parse().unwrap(),
    )
}

fn parse_wire_points<'a>(s: &'a str) -> impl Iterator<Item = Point> + 'a {
    s.split(',')
        .map(parse_movement)
        .flat_map(|(direction, count)| repeat(direction).take(count))
        .scan(CENTRAL_PORT_LOC, |p, direction| {
            *p = p.move_in_dir(direction);
            Some(*p)
        })
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum VisitStatus {
    Neither,
    First(usize),
    Second(usize),
    Both(usize),
}

impl VisitStatus {
    fn increment(&mut self, first: bool, dist: usize) -> &mut VisitStatus {
        *self = match (*self, first) {
            (VisitStatus::Neither, true) => VisitStatus::First(dist),
            (VisitStatus::Neither, false) => VisitStatus::Second(dist),
            (a @ VisitStatus::First(_), true) => a,
            (VisitStatus::First(d), false) => VisitStatus::Both(d + dist),
            (a @ VisitStatus::Second(_), _) => a,
            (a @ VisitStatus::Both(_), _) => a,
        };
        self
    }

    fn is_both(&self) -> bool {
        if let VisitStatus::Both(_) = self {
            true
        } else {
            false
        }
    }
}

impl Default for VisitStatus {
    fn default() -> Self {
        VisitStatus::Neither
    }
}

fn closest_intersection_dist<'a>(wires: &[&'a str]) -> i32 {
    let mut map: HashMap<_, VisitStatus> = Default::default();
    for (i, wire) in wires.into_iter().enumerate() {
        let first = i == 0;
        for (d, point) in parse_wire_points(wire).enumerate() {
            map.entry(point).or_default().increment(first, 1 + d);
        }
    }

    map.iter()
        .filter(|&(_, &status)| status.is_both())
        .map(|(p, _)| p.manhattan_dist(CENTRAL_PORT_LOC))
        .min()
        .expect("Found no intersection")
}

fn fewest_intersection<'a>(wires: &[&'a str]) -> usize {
    let mut map: HashMap<_, VisitStatus> = Default::default();
    for (i, wire) in wires.into_iter().enumerate() {
        let first = i == 0;
        for (d, point) in parse_wire_points(wire).enumerate() {
            map.entry(point).or_default().increment(first, 1 + d);
        }
    }

    map.iter()
        .filter_map(|(_, &status)| {
            if let VisitStatus::Both(steps) = status {
                Some(steps)
            } else {
                None
            }
        })
        .min()
        .expect("Found no intersection")
}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    println!("Hello, world!");

    let input = fs::read_to_string("input")?;
    let wires: Vec<_> = input.lines().collect();
    println!(
        "The Manhattan distance from the central port to the closest intersection is {}",
        closest_intersection_dist(&wires)
    );
    println!(
        "The fewest combined steps the wires must take to reach an intersection is {}",
        fewest_intersection(&wires)
    );
    Ok(())
}

#[test]
fn test_smallest_cross_dist() {
    assert_eq!(
        closest_intersection_dist(&["R8,U5,L5,D3", "U7,R6,D4,L4"]),
        6
    );
    assert_eq!(
        closest_intersection_dist(&[
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            "U62,R66,U55,R34,D71,R55,D58,R83"
        ]),
        159
    );
    assert_eq!(
        closest_intersection_dist(&[
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
        ]),
        135
    );
}

#[test]
fn test_fewest_steps_to_intersection() {
    assert_eq!(
        fewest_intersection(&[
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            "U62,R66,U55,R34,D71,R55,D58,R83"
        ]),
        610
    );
    assert_eq!(
        fewest_intersection(&[
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
        ]),
        410
    );
}
