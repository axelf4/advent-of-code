use std::collections::HashMap;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("input")?;
    let direct_orbits = input.lines().map(|l| {
        let mut splitted = l.split(')');
        (splitted.next().unwrap(), splitted.next().unwrap())
    });

    let mut objects: HashMap<&str, (usize, bool)> = Default::default();
    objects.insert("COM", (0, false));

    let mut dependants: HashMap<&str, Vec<&str>> = Default::default();

    let mut num_orbits = 0;

    fn update_orbits<'a>(
        num_orbits: &mut usize,
        objects: &mut HashMap<&'a str, (usize, bool)>,
        dependants: &mut HashMap<&'a str, Vec<&'a str>>,
        a: &'a str,
        b: &'a str,
    ) -> bool {
        let parent = objects.get(a);
        if let Some(&(parent_level, _)) = parent {
            *num_orbits += 1 + parent_level;
            objects.insert(b, (parent_level + 1, false));

            if let Some(deps) = dependants.get_mut(b) {
                let new: Vec<_> = deps.drain(..).collect();
                for b2 in new {
                    update_orbits(num_orbits, objects, dependants, b, b2);
                }
            }
            true
        } else {
            false
        }
    }

    for (a, b) in direct_orbits {
        if update_orbits(&mut num_orbits, &mut objects, &mut dependants, a, b) {
            //
        } else {
            dependants.entry(a).or_default().push(b);
        }
    }

    println!(
        "The total number of direct and indirect orbits is {}",
        num_orbits
    );

    // Part 2
    // Paint each node from SAN to root

    Ok(())
}

#[test]
fn test_simple_map() {
    let map = r"COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L";
    assert_eq!(num_orbits(map), 42);
}
