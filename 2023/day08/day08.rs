use std::collections::HashMap;
use std::error;
use std::fmt;
use std::fs;
use std::mem;

#[derive(Debug)]
struct ParseInputError;

impl fmt::Display for ParseInputError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse input")
    }
}

impl error::Error for ParseInputError {}

#[derive(Clone, Copy)]
enum Instruction {
    L,
    R,
}

fn count_steps(
    start: &str,
    f: impl Fn(&str) -> bool,
    mut instructions: impl Iterator<Item = Instruction>,
    nodes: &HashMap<&str, (&str, &str)>,
) -> u32 {
    let mut count = 0;
    let mut node = start;
    while !f(node) {
        count += 1;
        node = match instructions.next().unwrap() {
            Instruction::L => nodes[node].0,
            Instruction::R => nodes[node].1,
        };
    }
    count
}

fn gcd(mut a: u64, mut b: u64) -> u64 {
    while a != b {
        if a < b {
            mem::swap(&mut a, &mut b);
        }
        a -= b;
    }
    a
}

fn lcm(a: u64, b: u64) -> u64 {
    a * (b / gcd(a, b))
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let input = fs::read_to_string("input")?;
    let mut lines = input.lines();

    let instructions = {
        let xs: Vec<_> = lines
            .next()
            .ok_or(ParseInputError)?
            .as_bytes()
            .into_iter()
            .map(|x| {
                Ok(match x {
                    b'L' => Instruction::L,
                    b'R' => Instruction::R,
                    _ => return Err(ParseInputError),
                })
            })
            .collect::<Result<_, _>>()?;
        xs.into_iter().cycle()
    };
    lines.next().ok_or(ParseInputError)?;

    let nodes: HashMap<&str, (&str, &str)> = lines
        .map(|line| {
            let name = line.get(0..3).ok_or(ParseInputError)?;
            let left = line.get(7..10).ok_or(ParseInputError)?;
            let right = line.get(12..15).ok_or(ParseInputError)?;
            Ok((name, (left, right)))
        })
        .collect::<Result<_, ParseInputError>>()?;

    let solution1 = count_steps("AAA", |x| x == "ZZZ", instructions.clone(), &nodes);
    let solution2 = nodes
        .keys()
        .filter(|x| x.ends_with('A'))
        .map(|x| count_steps(x, |x| x.ends_with('Z'), instructions.clone(), &nodes) as u64)
        .fold(1, lcm);
    println!("Solution 1: {solution1}, solution 2: {solution2}");
    Ok(())
}
