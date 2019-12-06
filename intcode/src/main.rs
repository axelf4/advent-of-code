use std::convert::{TryFrom, TryInto};
use std::fs;
use std::io;

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

impl TryFrom<char> for ParameterMode {
    type Error = &'static str;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '0' => Ok(ParameterMode::Position),
            '1' => Ok(ParameterMode::Immediate),
            _ => Err("Bad parameter mode"),
        }
    }
}

struct ParameterModeList(Vec<ParameterMode>);

impl ParameterModeList {
    fn get(&self, i: usize) -> ParameterMode {
        self.0.get(i).copied().unwrap_or_default()
    }
}

fn parse_opcode_modes(value: i32) -> Result<(ParameterModeList, Opcode), &'static str> {
    let opcode = (value % 100).try_into()?;
    let modes = ParameterModeList(
        (value / 100)
            .to_string()
            .chars()
            .rev()
            .map(TryInto::try_into)
            .collect::<Result<_, _>>()?,
    );

    Ok((modes, opcode))
}

fn arg<'a>(mem: &'a mut [i32], modes: &ParameterModeList, ip: usize, i: usize) -> &'a mut i32 {
    let mode = modes.get(i);
    match mode {
        ParameterMode::Position => &mut mem[mem[ip + 1 + i] as usize],
        ParameterMode::Immediate => &mut mem[ip + 1 + i],
    }
}

fn interpret(mem: &mut [i32]) -> i32 {
    let mut ip = 0; // Instruction pointer
    loop {
        let (modes, opcode) = parse_opcode_modes(mem[ip]).unwrap();
        let save_ip = ip;
        match opcode {
            Opcode::Add => {
                // a, b, out
                let result = *arg(mem, &modes, ip, 0) + *arg(mem, &modes, ip, 1);
                *arg(mem, &modes, ip, 2) = result;
            }
            Opcode::Multiply => {
                // a, b, out
                let result = *arg(mem, &modes, ip, 0) * *arg(mem, &modes, ip, 1);
                *arg(mem, &modes, ip, 2) = result;
            }
            Opcode::Halt => break mem[0],
            Opcode::Input => {
                assert_eq!(modes.get(0), ParameterMode::Position);
                let mut input_text = String::new();
                io::stdin()
                    .read_line(&mut input_text)
                    .expect("Failed to read from stdin");
                *arg(mem, &modes, ip, 0) =
                    input_text.trim().parse().expect("Failed to parse input");
            }
            Opcode::JumpIfTrue => {
                if *arg(mem, &modes, ip, 0) != 0 {
                    ip = (*arg(mem, &modes, ip, 1)).try_into().unwrap();
                }
            }
            Opcode::JumpIfFalse => {
                if *arg(mem, &modes, ip, 0) == 0 {
                    ip = (*arg(mem, &modes, ip, 1)).try_into().unwrap();
                }
            }
            Opcode::LessThan => {
                *arg(mem, &modes, ip, 2) = if *arg(mem, &modes, ip, 0) < *arg(mem, &modes, ip, 1) {
                    1
                } else {
                    0
                }
            }
            Opcode::Equals => {
                *arg(mem, &modes, ip, 2) = if *arg(mem, &modes, ip, 0) == *arg(mem, &modes, ip, 1) {
                    1
                } else {
                    0
                }
            }
            Opcode::Output => {
                println!("{}", *arg(mem, &modes, ip, 0));
            }
        }
        // If instruction modified ip: Do not automatically increase
        if ip == save_ip {
            ip += 1 + opcode.arg_count();
        }
    }
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

fn run_program(input: &str) -> Memory {
    let mut mem = mem_from_string(input);
    interpret(&mut mem);
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
            (noun, verb, interpret(&mut mem))
        })
        .find(|&(_, _, output)| output == 19690720)
    {
        println!("The noun is {}, verb is {}", noun, verb);
    }

    mem[1] = 12;
    mem[2] = 2;

    let output = interpret(&mut mem);
    println!("The output is: {output}", output = output);

    println!("Running TEST");
    run_program(&fs::read_to_string("input5")?);

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
