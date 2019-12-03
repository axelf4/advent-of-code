use std::convert::{TryFrom, TryInto};
use std::fs;

enum Op {
    Add,
    Multiply,
    Halt,
}

impl TryFrom<usize> for Op {
    type Error = &'static str;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Op::Add),
            2 => Ok(Op::Multiply),
            99 => Ok(Op::Halt),
            _ => Err("Bad opcode"),
        }
    }
}

fn interpret(mem: &mut [usize]) -> usize {
    let mut ip = 0; // Instruction pointer
    loop {
        let op = mem[ip].try_into().unwrap();
        match op {
            Op::Add => {
                if let [a, b, out] = mem[ip + 1..ip + 4] {
                    mem[out] = mem[a] + mem[b];
                } else {
                    unreachable!()
                }
                ip += 4;
            }
            Op::Multiply => {
                if let [a, b, out] = mem[ip + 1..ip + 4] {
                    mem[out] = mem[a] * mem[b];
                } else {
                    unreachable!()
                }
                ip += 4;
            }
            Op::Halt => break mem[0],
        }
    }
}

fn main() -> std::io::Result<()> {
    println!("Hello, world!");
    let input = fs::read_to_string("input")?;
    let mut mem: Vec<usize> = input
        .trim()
        .split(',')
        .map(str::parse)
        .collect::<Result<_, _>>()
        .unwrap();

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

    Ok(())
}
