use std::fs;
use std::io;
use std::iter;

mod intcode;

use intcode::{mem_from_string, Interpreter, Memory, Vm};

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
}
