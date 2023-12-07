# AOC 2023 day 7 in Roc

app "day07"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, pf.Task, pf.Path, pf.File]
    provides [main] to pf

parseLine = \line -> Result.try (Str.splitFirst line " ") \{ before, after } ->
    Result.map (Str.toNat after) \bid -> {
        hand: Str.toUtf8 before |> List.map (\x -> when x is
            'A' -> 14
            'K' -> 13
            'Q' -> 12
            # 'J' -> 11 # Part 1
            'J' -> 1 # Part 2
            'T' -> 10
            _ -> x - '0'),
        bid
    }
parse = \s -> Str.trim s |> Str.split "\n" |> List.mapTry parseLine

group : List a -> List (Num *) where a implements Eq
group = \xs -> List.walk xs { counts: [], prev: None } \st, x ->
    when st.prev is
        None -> { counts: [1], prev: Some x }
        Some y if x == y ->
            { st & counts: List.update st.counts (List.len st.counts - 1) \n -> n + 1 }
        _ -> { counts: List.append st.counts 1, prev: Some x }
    |> .counts

handType = \hand -> when List.sortAsc hand |> group |> List.sortDesc is
    [5] -> 6 # Five of a kind
    [4, 1] -> 5 # Four of a kind
    [3, 2] -> 4 # Full house
    [3, 1, 1] -> 3 # Three of a kind
    [2, 2, 1] -> 2 # Two pair
    [2, 1, 1, 1] -> 1 # One pair
    _ -> 0 # High card
jokerHandType = \hand ->
    nonJokers = List.dropIf hand \x -> x == 1
    when List.map nonJokers
            (\x -> List.map hand (\y -> if y == 1 then x else y) |> handType)
        |> List.max is
        Ok x -> x
        Err ListWasEmpty -> handType hand

cmpHands = \{hand: a}, {hand: b} ->
    aType = jokerHandType a
    bType = jokerHandType b
    if aType < bType then LT
    else if aType > bType then GT
    else when List.map2 a b Pair |> List.findFirst \Pair x y -> x != y is
        Ok (Pair x y) -> if x < y then LT else GT
        Err NotFound -> EQ

solve = \input -> List.sortWith input cmpHands
    |> List.mapWithIndex \{ bid: b }, i -> b * (1 + i)
    |> List.sum

run =
    s <- File.readUtf8 (Path.fromStr "input") |> Task.await
    input <- Task.fromResult (parse s) |> Task.await
    Stdout.line "Solution: \(Num.toStr (solve input))"
main = Task.mapErr run (\_ -> 1)
