use std::convert::{TryFrom, TryInto};
use std::fs;
use std::io;
use std::iter::{self, FusedIterator};

#[derive(Debug, Copy, Clone)]
enum Opcode {
    Add = 1,
    Multiply = 2,
    Input = 3,
    Output = 4,
    JumpIfTrue = 5,
    JumpIfFalse = 6,
    LessThan = 7,
    Equals = 8,
    Halt = 99,
}

impl Opcode {
    #[allow(dead_code)]
    fn arg_count(&self) -> usize {
        match self {
            Opcode::Add => 3,
            Opcode::Multiply => 3,
            Opcode::Input => 1,
            Opcode::Output => 1,
            Opcode::JumpIfTrue => 2,
            Opcode::JumpIfFalse => 2,
            Opcode::LessThan => 3,
            Opcode::Equals => 3,
            Opcode::Halt => 0,
        }
    }
}

impl TryFrom<i32> for Opcode {
    type Error = &'static str;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Opcode::Add),
            2 => Ok(Opcode::Multiply),
            3 => Ok(Opcode::Input),
            4 => Ok(Opcode::Output),
            5 => Ok(Opcode::JumpIfTrue),
            6 => Ok(Opcode::JumpIfFalse),
            7 => Ok(Opcode::LessThan),
            8 => Ok(Opcode::Equals),
            99 => Ok(Opcode::Halt),
            _ => Err("Bad opcode"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ParameterMode {
    Position = 0,
    Immediate = 1,
}

impl Default for ParameterMode {
    fn default() -> Self {
        ParameterMode::Position
    }
}

impl TryFrom<i32> for ParameterMode {
    type Error = &'static str;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ParameterMode::Position),
            1 => Ok(ParameterMode::Immediate),
            _ => Err("Bad parameter mode"),
        }
    }
}

struct Digits(i32);

impl Iterator for Digits {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            None
        } else {
            let digit = self.0 % 10;
            self.0 /= 10;
            Some(digit)
        }
    }
}

impl FusedIterator for Digits {}

fn parse_opcode_modes(value: i32) -> Result<(impl Iterator<Item = ParameterMode>, Opcode), &'static str> {
    let opcode = (value % 100).try_into()?;
    let modes = Digits(value / 100)
            .map(TryInto::try_into)
            .map(Result::unwrap)
            .chain(iter::repeat(Default::default()));

    Ok((modes, opcode))
}

struct Argument(usize, ParameterMode);

impl Argument {
    fn get_mut<'a>(&self, mem: &'a mut [i32]) -> &'a mut i32 {
        match self.1 {
            ParameterMode::Position => &mut mem[mem[self.0] as usize],
            ParameterMode::Immediate => &mut mem[self.0],
        }
    }
}

trait NextTupleExt: Iterator {
    fn next_two(&mut self) -> (Self::Item, Self::Item) {
        (self.next().unwrap(), self.next().unwrap())
    }

    fn next_three(&mut self) -> (Self::Item, Self::Item, Self::Item) {
        (self.next().unwrap(), self.next().unwrap(), self.next().unwrap())
    }
}

impl<I: Iterator> NextTupleExt for I {}

fn interpret<'a>(mem: &'a mut [i32], input: &'a mut impl Iterator<Item = i32>) -> impl Iterator<Item = i32> + 'a {
    let mut ip = 0; // Instruction pointer
    iter::from_fn(move || {
        loop {
            let (modes, opcode) = parse_opcode_modes(mem[ip]).unwrap();
            ip += 1;

            let mut args = iter::from_fn(|| {
                let arg = ip;
                ip += 1;
                Some(arg)
            }).zip(modes).map(|(arg, mode)| Argument(arg, mode));

            match opcode {
                Opcode::Add => {
                    let (a, b, out) = args.next_three();
                    *out.get_mut(mem) = *a.get_mut(mem) + *b.get_mut(mem);
                }
                Opcode::Multiply => {
                    let (a, b, out) = args.next_three();
                    *out.get_mut(mem) = *a.get_mut(mem) * *b.get_mut(mem);
                }
                Opcode::Halt => break None,
                Opcode::Input => {
                    let out = args.next().unwrap();
                    assert_eq!(out.1, ParameterMode::Position);
                    *out.get_mut(mem) = input.next().unwrap();
                }
                Opcode::JumpIfTrue => {
                    let (a, b) = args.next_two();
                    if *a.get_mut(mem) != 0 {
                        ip = (*b.get_mut(mem)).try_into().unwrap();
                    }
                }
                Opcode::JumpIfFalse => {
                    let (a, b) = args.next_two();
                    if *a.get_mut(mem) == 0 {
                        ip = (*b.get_mut(mem)).try_into().unwrap();
                    }
                }
                Opcode::LessThan => {
                    let (a, b, out) = args.next_three();
                    *out.get_mut(mem) = if *a.get_mut(mem) < *b.get_mut(mem) { 1 } else {0};
                }
                Opcode::Equals => {
                    let (a, b, out) = args.next_three();
                    *out.get_mut(mem) = if *a.get_mut(mem) == *b.get_mut(mem) { 1 } else {0};
                }
                Opcode::Output => {
                    let a = args.next().unwrap();
                    break Some(*a.get_mut(mem));
                }
            }
        }
    })
}

type Memory = Vec<i32>;

fn mem_from_string(input: &str) -> Memory {
    input
        .trim()
        .split(',')
        .map(str::parse)
        .collect::<Result<_, _>>()
        .unwrap()
}

fn input_iter() -> impl Iterator<Item = i32> {
    iter::from_fn(|| {
        let mut input_text = String::new();
        io::stdin()
            .read_line(&mut input_text)
            .expect("Failed to read from stdin");
        Some(input_text.trim().parse().expect("Failed to parse input"))
    })
}

fn run_program(input: &str) -> Memory {
    let mut mem = mem_from_string(input);
    let mut input_iter = input_iter();
    let output = interpret(&mut mem, &mut input_iter);
    println!("Output is {:?}", output.collect::<Vec<_>>());
    mem
}

fn main() -> std::io::Result<()> {
    println!("Hello, world!");
    let input = fs::read_to_string("input")?;
    let mut mem = mem_from_string(&input);

    if let Some((noun, verb, _)) = (0..=99)
        .flat_map(|noun| (0..=99).map(move |verb| (noun, verb)))
        .map(|(noun, verb)| {
            let mut mem = mem.clone();
            mem[1] = noun;
            mem[2] = verb;
            let mut input_iter = iter::empty();
            for _ in interpret(&mut mem, &mut input_iter) {}
            (noun, verb, mem[0])
        })
        .find(|&(_, _, output)| output == 19690720)
    {
        println!("The noun is {}, verb is {}", noun, verb);
    }

    mem[1] = 12;
    mem[2] = 2;

    let mut input_iter = iter::empty();
    let output = interpret(&mut mem, &mut input_iter);
    println!("The output is: {output:?}", output = output.collect::<Vec<_>>());

    // println!("Running TEST");
    // run_program(&fs::read_to_string("input5")?);

    Ok(())
}

#[test]
fn test_small_programs() {
    assert_eq!(run_program("1,0,0,0,99"), [2, 0, 0, 0, 99]);
    assert_eq!(run_program("2,3,0,3,99"), [2, 3, 0, 6, 99]);
    assert_eq!(run_program("2,4,4,5,99,0"), [2, 4, 4, 5, 99, 9801]);
    assert_eq!(
        run_program("1,1,1,4,99,5,6,0,99"),
        [30, 1, 1, 4, 2, 5, 6, 0, 99]
    );
}

#[test]
fn test_parameter_modes() {
    let mut input = iter::once(2);
    assert_eq!(interpret(&mut mem_from_string("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"),
        &mut input).collect::<Vec<_>>(), [1]);

    let mut input = iter::once(0);
    assert_eq!(interpret(&mut mem_from_string("3,3,1105,-1,9,1101,0,0,12,4,12,99,1"),
        &mut input).collect::<Vec<_>>(), [0]);
}
