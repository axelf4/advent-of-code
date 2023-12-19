defmodule Day19 do
  import NimbleParsec

  category = choice([replace(string("x"), :x),
                     replace(string("m"), :m),
                     replace(string("a"), :a),
                     replace(string("s"), :s)])

  verdict = choice([replace(string("A"), :A), replace(string("R"), :R),
                    ascii_string([?a..?z], min: 1)])

  rule = choice([
    category
    |> choice([replace(string("<"), :<), replace(string(">"), :>)])
    |> integer(min: 1)
    |> ignore(string(":"))
    |> concat(verdict)
    |> reduce({List, :to_tuple, []}),
    verdict
  ])

  defp process_workflow(rest, [rules: rules, name: name], context, _line, _offset) do
    {rest, [{name, rules}], context}
  end

  workflow =
    unwrap_and_tag(ascii_string([?a..?z], min: 1), :name)
    |> ignore(string("{"))
    |> tag(rule |> repeat(ignore(string(",")) |> concat(rule)), :rules)
    |> ignore(string("}"))
    |> post_traverse(:process_workflow)

  defp process_part_rating(rest, [rating, category], context, _line, _offset) do
    {rest, [{category, rating}], context}
  end

  part_rating =
    category
    |> ignore(string("="))
    |> integer(min: 1)
    |> post_traverse(:process_part_rating)

  part_ratings =
    ignore(string("{"))
    |> concat(part_rating) |> repeat(ignore(string(",")) |> concat(part_rating))
    |> ignore(string("}"))
    |> reduce({Map, :new, []})

  defparsec :input,
    unwrap_and_tag(repeat(workflow |> ignore(ascii_char([?\n])))
      |> reduce({Map, :new, []}), :workflows)
    |> ignore(ascii_char([?\n]))
    |> tag(repeat(part_ratings |> ignore(ascii_char([?\n]))), :ratings)
    |> eos

  defp is_accepted(workflows, part, [{category, op, value, verdict} | xs]) do
    x = part[category]
    matched = case op do
      :< -> x < value
      :> -> x > value
    end
    is_accepted(workflows, part, if(matched, do: [verdict], else: xs))
  end
  defp is_accepted(_, _, [:A]), do: true
  defp is_accepted(_, _, [:R]), do: false
  defp is_accepted(workflows, part, [workflow | _]),
    do: is_accepted(workflows, part, workflows[workflow])

  defp solve2(workflows, x, [{category, op, value, verdict} | xs]) do
    range = x[category]
    {below, above} = cond do
      value < range.min -> {%{ min: 0, max: 0 }, range}
      value > range.max -> {range, %{ min: 0, max: 0 }}
      true ->
        {%{ min: range.min, max: value - if(op == :<, do: 1, else: 0) },
         %{ min: value + if(op == :<, do: 0, else: 1), max: range.max }}
    end

    {matched, not_matched} = case op do
                               :< -> {below, above}
                               :> -> {above, below}
                             end
    solve2(workflows, %{ x | category => matched }, [verdict]) +
    solve2(workflows, %{ x | category => not_matched }, xs)
  end
  defp solve2(_, x, [:A]), do:
    (x.x.max - x.x.min + 1) * (x.m.max - x.m.min + 1) *
    (x.a.max - x.a.min + 1) * (x.s.max - x.s.min + 1)
  defp solve2(_, _, [:R]), do: 0
  defp solve2(workflows, x, [workflow | _]),
    do: solve2(workflows, x, workflows[workflow])

  def main(_args \\ []) do
    {:ok, s} = File.read("input")
    {:ok, [workflows: workflows, ratings: parts], "", _, _, _} = input(s)

    part1 =
      Enum.filter(parts, &is_accepted(workflows, &1, workflows["in"]))
      |> Enum.map(fn x -> x.x + x.m + x.a + x.s end) |> Enum.sum

    range = %{ min: 1, max: 4000 }
    initial = %{ x: range, m: range, a: range, s: range }
    part2 = solve2(workflows, initial, workflows["in"])

    :io.format("Part 1: ~B~nPart 2: ~B~n", [part1, part2])
  end
end
