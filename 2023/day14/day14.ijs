NB. AOC 2023 day 14 in J

input =: ];._2 (1!:1 <'input')

tilt =: {{ ((-. x) & *. @: (-. *. 1 & (|.!.0)) +. (*. |.!.1 @: +. & x)) ^:_ y }}
tiltN =: ('#'=input) & tilt
tiltS =: (|. '#'=input) & tilt &.: |.
tiltW =: (|: '#'=input) & tilt &.: |:
tiltE =: (|. |: '#'=input) & tilt &.: (|. @: |:)

load =: +/ @: , @: *. & (1+ |: ($input) $ i.-{.$input) NB. Calculates total load
spin =: tiltE @: tiltS @: tiltW @: tiltN NB. Performs one spin cycle

part1 =: load tiltN 'O'=input

n =: 1000000000 [ m =: 1000
tab =: spin ^:(<m+1) 'O'=input
period =: m - (}: tab) i: {: tab
part2 =: load spin ^:(period | n - m) {: tab
