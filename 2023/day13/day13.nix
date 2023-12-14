# AOC 2023 day 13 in Nix

let
  pkgs = import <nixpkgs> {};
  inherit (builtins) add length elemAt foldl' genList;
  inherit (pkgs) lib;
  inherit (lib.lists) head tail take drop range reverseList zipListsWith findFirst;
  inherit (lib.strings) splitString stringToCharacters;

  sum = foldl' builtins.add 0;
  transpose = xs: genList (i: map (x: elemAt x i) xs) (length (head xs));
  splitList = x: xs: let
    result = foldl'
    ({ acc, cur }: y: if y == x then { acc = acc ++ [cur]; cur = []; }
                      else { inherit acc; cur = cur ++ [y]; })
    { acc = []; cur = []; } xs;
  in result.acc ++ (if length result.cur == 0 then [] else [result.curr]);

  input = builtins.readFile ./input;
  lines = splitString "\n" input;
  patterns = splitList "" lines;

  mirrorIndex = pattern: findFirst
    (i: sum (zipListsWith
      (as: bs: sum (zipListsWith (a: b: if a == b then 0 else 1) as bs))
      (reverseList (take i pattern)) (drop i pattern))
    # == 0) # Part 1
    == 1) # Part 2
    null (range 1 (length pattern - 1));

  patternNote = pattern: let
    xs = map stringToCharacters pattern;
    v = mirrorIndex (transpose xs);
    h = mirrorIndex xs;
  in if v != null then v else 100 * h;
in sum (map patternNote patterns)
