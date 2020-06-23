use std::convert::{TryFrom, TryInto};
use std::iter::{self, FusedIterator};
use std::num::ParseIntError;
use std::str::FromStr;

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
    RelativeBaseOffset = 9,
    Halt = 99,
}

impl TryFrom<i64> for Opcode {
    type Error = &'static str;
    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Opcode::Add),
            2 => Ok(Opcode::Multiply),
            3 => Ok(Opcode::Input),
            4 => Ok(Opcode::Output),
            5 => Ok(Opcode::JumpIfTrue),
            6 => Ok(Opcode::JumpIfFalse),
            7 => Ok(Opcode::LessThan),
            8 => Ok(Opcode::Equals),
            9 => Ok(Opcode::RelativeBaseOffset),
            99 => Ok(Opcode::Halt),
            _ => Err("Bad opcode"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ParameterMode {
    Position = 0,
    Immediate = 1,
    Relative = 2,
}

impl Default for ParameterMode {
    fn default() -> Self {
        ParameterMode::Position
    }
}

impl TryFrom<i64> for ParameterMode {
    type Error = &'static str;
    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ParameterMode::Position),
            1 => Ok(ParameterMode::Immediate),
            2 => Ok(ParameterMode::Relative),
            _ => Err("Bad parameter mode"),
        }
    }
}

struct Digits(i64);

impl Iterator for Digits {
    type Item = i64;

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

fn parse_opcode_modes(
    value: i64,
) -> Result<(impl Iterator<Item = ParameterMode>, Opcode), &'static str> {
    let opcode = (value % 100).try_into()?;
    let modes = Digits(value / 100)
        .map(TryInto::try_into)
        .map(Result::unwrap)
        .chain(iter::repeat(Default::default()));

    Ok((modes, opcode))
}

trait NextTupleExt: Iterator {
    fn next_two(&mut self) -> (Self::Item, Self::Item) {
        (self.next().unwrap(), self.next().unwrap())
    }

    fn next_three(&mut self) -> (Self::Item, Self::Item, Self::Item) {
        (
            self.next().unwrap(),
            self.next().unwrap(),
            self.next().unwrap(),
        )
    }
}

impl<I: Iterator> NextTupleExt for I {}

pub type Memory = Vec<i64>;

pub fn mem_from_string(input: &str) -> Result<Memory, ParseIntError> {
    input.trim().split(',').map(str::parse).collect()
}

#[derive(Debug, Clone)]
pub struct Vm {
    pub mem: Memory,
    /// Instruction pointer.
    ip: usize,
    relative_base: i64,
}

impl Vm {
    pub fn from_mem(mem: Memory) -> Self {
        Vm {
            mem,
            ip: 0,
            relative_base: 0,
        }
    }
}

impl FromStr for Vm {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        mem_from_string(s).map(Self::from_mem)
    }
}

#[derive(Debug, Copy, Clone)]
struct Argument(usize, ParameterMode);

impl Argument {
    fn get_mut<'a>(
        &self,
        Vm {
            mem, relative_base, ..
        }: &'a mut Vm,
    ) -> &'a mut i64 {
        let address = match self.1 {
            ParameterMode::Position => mem[self.0] as usize,
            ParameterMode::Immediate => self.0,
            ParameterMode::Relative => (mem[self.0] + *relative_base) as usize,
        };
        if address >= mem.len() {
            mem.resize(address + 1, 0); // Fill in new memory with the value 0
        }
        &mut mem[address]
    }
}

pub struct Interpreter<I>(pub Vm, pub I);

impl<I: Iterator<Item = i64>> Iterator for Interpreter<I> {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        let Interpreter(vm, input) = self;
        loop {
            let (modes, opcode) = parse_opcode_modes(vm.mem[vm.ip]).unwrap();
            vm.ip += 1;

            let mut args = iter::from_fn(|| {
                let arg = vm.ip;
                vm.ip += 1;
                Some(arg)
            })
            .zip(modes)
            .map(|(arg, mode)| Argument(arg, mode));

            match opcode {
                Opcode::Add => {
                    let (a, b, out) = args.next_three();
                    *out.get_mut(vm) = *a.get_mut(vm) + *b.get_mut(vm);
                }
                Opcode::Multiply => {
                    let (a, b, out) = args.next_three();
                    *out.get_mut(vm) = *a.get_mut(vm) * *b.get_mut(vm);
                }
                Opcode::Halt => break None,
                Opcode::Input => {
                    let out = args.next().unwrap();
                    assert_ne!(out.1, ParameterMode::Immediate);
                    *out.get_mut(vm) = input.next().expect("No input");
                }
                Opcode::JumpIfTrue => {
                    let (a, b) = args.next_two();
                    if *a.get_mut(vm) != 0 {
                        vm.ip = (*b.get_mut(vm)).try_into().unwrap();
                    }
                }
                Opcode::JumpIfFalse => {
                    let (a, b) = args.next_two();
                    if *a.get_mut(vm) == 0 {
                        vm.ip = (*b.get_mut(vm)).try_into().unwrap();
                    }
                }
                Opcode::LessThan => {
                    let (a, b, out) = args.next_three();
                    *out.get_mut(vm) = if *a.get_mut(vm) < *b.get_mut(vm) {
                        1
                    } else {
                        0
                    };
                }
                Opcode::Equals => {
                    let (a, b, out) = args.next_three();
                    *out.get_mut(vm) = if *a.get_mut(vm) == *b.get_mut(vm) {
                        1
                    } else {
                        0
                    };
                }
                Opcode::Output => {
                    let a = args.next().unwrap();
                    break Some(*a.get_mut(vm));
                }
                Opcode::RelativeBaseOffset => {
                    let a = args.next().unwrap();
                    vm.relative_base += *a.get_mut(vm);
                }
            }
        }
    }
}

impl<I> Interpreter<I>
where
    Self: Iterator,
{
    pub fn run(&mut self) {
        self.by_ref().for_each(drop)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_relative_base() {
        let mem =
            mem_from_string("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99").unwrap();
        assert_eq!(
            Interpreter(Vm::from_mem(mem.clone()), iter::empty()).collect::<Vec<_>>(),
            mem
        );

        let output = Interpreter(
            "1102,34915192,34915192,7,4,7,99,0".parse().unwrap(),
            iter::empty(),
        )
        .next()
        .unwrap();
        assert_eq!(Digits(output).count(), 16);

        let mem = mem_from_string("104,1125899906842624,99").unwrap();
        let mid = mem[1];
        assert_eq!(
            Interpreter(Vm::from_mem(mem), iter::empty())
                .next()
                .unwrap(),
            mid
        );
    }
}
