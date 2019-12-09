use std::convert::{TryFrom, TryInto};
use std::fs;
use std::io;
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

type Memory = Vec<i64>;

fn mem_from_string(input: &str) -> Result<Memory, ParseIntError> {
    input.trim().split(',').map(str::parse).collect()
}

#[derive(Debug, Clone)]
struct Vm {
    mem: Memory,
    /// Instruction pointer.
    ip: usize,
    relative_base: i64,
}

impl Vm {
    fn from_mem(mem: Memory) -> Self {
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
    fn get_mut<'a>(&self, vm: &'a mut Vm) -> &'a mut i64 {
        let Vm {
            ref mut mem,
            relative_base,
            ..
        } = *vm;
        let address = match self.1 {
            ParameterMode::Position => mem[self.0] as usize,
            ParameterMode::Immediate => self.0,
            ParameterMode::Relative => (mem[self.0] + relative_base) as usize,
        };
        if address >= mem.len() {
            mem.resize(address + 1, 0); // Fill in new memory with the value 0
        }
        &mut mem[address]
    }
}

struct Interpreter<I>(Vm, I);

impl<I: Iterator<Item = i64>> Iterator for Interpreter<I> {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        let Interpreter(ref mut vm, ref mut input) = *self;
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
    fn run(&mut self) {
        self.by_ref().for_each(drop)
    }
}

#[allow(dead_code)]
fn input_iter() -> impl Iterator<Item = i64> {
    iter::from_fn(|| {
        let mut input_text = String::new();
        io::stdin()
            .read_line(&mut input_text)
            .expect("Failed to read from stdin");
        Some(input_text.trim().parse().expect("Failed to parse input"))
    })
}

fn amplification_circuit(program: &str) -> i64 {
    use permutohedron::Heap;

    let mem = mem_from_string(program).unwrap();

    Heap::new(&mut [0, 1, 2, 3, 4])
        .map(|permutation| {
            permutation.iter().fold(0, |input_signal, &phase_setting| {
                let mem = mem.clone();
                let input = vec![phase_setting, input_signal].into_iter();
                let mut interpreter = Interpreter(Vm::from_mem(mem), input);
                let output_signal = interpreter
                    .next()
                    .expect("No output signal from the amplifier");
                assert!(interpreter.next().is_none());
                output_signal
            })
        })
        .max()
        .unwrap()
}

fn amplification_circuit_two(program: &str) -> i64 {
    use permutohedron::Heap;

    let mem = mem_from_string(program).unwrap();

    Heap::new(&mut [5, 6, 7, 8, 9])
        .map(|permutation| {
            let mut array: Vec<_> = permutation
                .iter()
                .zip(iter::repeat_with(|| Vm::from_mem(mem.clone())))
                .collect();

            let mut first = true;
            iter::successors(Some(0), move |&input_signal| {
                let output_signal = array.iter_mut().try_fold(
                    input_signal,
                    |input_signal, (&phase_setting, vm)| {
                        let input = if first {
                            vec![phase_setting, input_signal]
                        } else {
                            vec![input_signal]
                        }
                        .into_iter();
                        let mut interpreter = Interpreter(vm.clone(), input);
                        let output_signal = interpreter.next();
                        *vm = interpreter.0;
                        output_signal
                    },
                );
                first = false;
                output_signal
            })
            .last()
            .unwrap()
        })
        .max()
        .unwrap()
}

fn main() -> std::io::Result<()> {
    let mut vm: Vm = fs::read_to_string("input")?.parse().unwrap();

    if let Some((noun, verb, _)) = (0..=99)
        .flat_map(|noun| (0..=99).map(move |verb| (noun, verb)))
        .map(|(noun, verb)| {
            let mut interpreter = Interpreter(vm.clone(), iter::empty());
            interpreter.0.mem[1] = noun;
            interpreter.0.mem[2] = verb;
            interpreter.run();
            (noun, verb, interpreter.0.mem[0])
        })
        .find(|&(_, _, output)| output == 19690720)
    {
        println!("The noun is {}, verb is {}", noun, verb);
    }

    vm.mem[1] = 12;
    vm.mem[2] = 2;

    let mut interpreter = Interpreter(vm, iter::empty());
    interpreter.run();
    println!("The output is: {:?}", output = interpreter.0.mem[0]);

    // println!("Running TEST");
    // run_program(&fs::read_to_string("input5")?);

    let amp_program = fs::read_to_string("input7")?;
    println!(
        "The highest signal that can be sent to the thrusters is {}",
        amplification_circuit(&amp_program)
    );
    println!(
        "The higher highest signal that can be sent to the thrusters is {}",
        amplification_circuit_two(&amp_program)
    );

    // Day 9
    let mem = mem_from_string(&fs::read_to_string("input9")?).unwrap();
    println!(
        "Output of BOOST program test mode: {:?}",
        Interpreter(Vm::from_mem(mem.clone()), iter::once(1)).collect::<Vec<_>>()
    );
    println!(
        "Output of BOOST program boost mode: {:?}",
        Interpreter(Vm::from_mem(mem), iter::once(2)).collect::<Vec<_>>()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_program(input: &str) -> Memory {
        let mut interpreter = Interpreter(input.parse().unwrap(), iter::empty());
        interpreter.run();
        interpreter.0.mem
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
        assert_eq!(
            Interpreter(
                "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".parse().unwrap(),
                iter::once(2)
            )
            .collect::<Vec<_>>(),
            [1]
        );

        assert_eq!(
            Interpreter(
                "3,3,1105,-1,9,1101,0,0,12,4,12,99,1".parse().unwrap(),
                iter::once(2)
            )
            .collect::<Vec<_>>(),
            [1]
        );
    }

    #[test]
    fn test_amplification_circuit() {
        assert_eq!(
            amplification_circuit("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"),
            43210
        );
        assert_eq!(
            amplification_circuit(
                "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
            ),
            54321
        );
        assert_eq!(amplification_circuit("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"), 65210);
    }

    #[test]
    fn test_amplification_circuit_two() {
        assert_eq!(amplification_circuit_two("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"), 139629729);
        assert_eq!(amplification_circuit_two("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"), 18216);
    }

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
