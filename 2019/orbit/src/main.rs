use std::collections::HashMap;
use std::fs;
use std::iter::successors;

type ObjectKey<'a> = &'a str;

#[derive(Debug)]
struct Object<'a> {
    parent: Option<ObjectKey<'a>>,
    level: usize,
    /// Whether this object is a parent of Santa.
    santa_parent: bool,
}

fn build_tree(input: &str) -> (usize, HashMap<&str, Object>) {
    let direct_orbits = input.lines().map(|l| {
        let mut splitted = l.split(')');
        (splitted.next().unwrap(), splitted.next().unwrap())
    });

    let mut objects: HashMap<&str, Object> = Default::default();
    objects.insert(
        "COM",
        Object {
            parent: None,
            level: 0,
            santa_parent: false,
        },
    );
    let mut dependants: HashMap<&str, Vec<&str>> = Default::default();

    let mut num_orbits = 0;

    fn update_orbits<'a>(
        num_orbits: &mut usize,
        objects: &mut HashMap<&'a str, Object<'a>>,
        dependants: &mut HashMap<&'a str, Vec<&'a str>>,
        a: &'a str,
        b: &'a str,
    ) -> bool {
        let parent = objects.get(a);
        if let Some(&Object {
            level: parent_level,
            ..
        }) = parent
        {
            *num_orbits += 1 + parent_level;
            objects.insert(
                b,
                Object {
                    parent: Some(a),
                    level: parent_level + 1,
                    santa_parent: false,
                },
            );

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
        if !update_orbits(&mut num_orbits, &mut objects, &mut dependants, a, b) {
            dependants.entry(a).or_default().push(b);
        }
    }

    (num_orbits, objects)
}

fn num_orbital_transfers<'a>(mut objects: HashMap<&'a str, Object<'a>>) -> usize {
    // Paint each node from SAN to root
    let mut object = Some(objects.get_mut("SAN").expect("No Santa"));
    while let Some(o) = object {
        o.santa_parent = true;
        object = o.parent.and_then(|p| objects.get_mut(p));
    }

    // Find first common ancestor of YOU and SAN
    let santa = &objects["SAN"];
    let you = &objects["YOU"];
    let mut you_to_root_path = successors(Some(you), |o| o.parent.map(|p| &objects[p]));
    let first_ancestor = you_to_root_path
        .find(|o| o.santa_parent)
        .expect("No path to Santa");

    (you.level - first_ancestor.level - 1) + (santa.level - first_ancestor.level - 1)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("input")?;
    let (num_orbits, objects) = build_tree(&input);
    println!(
        "The total number of direct and indirect orbits is {}",
        num_orbits,
    );

    println!(
        "The minimum number of orbital transfers required is {}",
        num_orbital_transfers(objects)
    );

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
    let (num_orbits, _) = build_tree(map);
    assert_eq!(num_orbits, 42);
}

#[test]
fn test_orbital_transfers() {
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
K)L
K)YOU
I)SAN";
    let (_, objects) = build_tree(map);
    assert_eq!(num_orbital_transfers(objects), 4);
}
