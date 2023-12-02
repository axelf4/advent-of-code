-module(day02).
-export([solve/0]).

parse_line(Str) ->
	{match, [Id, Rest]} = re:run(Str, <<"Game (\\d+): (.*)$">>,
								 [anchored, {capture, all_but_first, binary}]),
	{binary_to_integer(Id), parse_sets(Rest, [])}.
parse_sets(Str, Acc) ->
	{Subset, Rest} = parse_subset(Str, #{}),
	Acc2 = [Subset | Acc],
	case Rest of
		<<"; ", Rest2/binary>> -> parse_sets(Rest2, Acc2);
		<<>> -> Acc2
	end.
parse_subset(Str, Acc) ->
	{Count, <<" ", Rest1/binary>>} = string:to_integer(Str),
	{Color, Rest2}
		= case Rest1 of
			  <<"red", Xs/binary>> -> {red, Xs};
			  <<"green", Xs/binary>> -> {green, Xs};
			  <<"blue", Xs/binary>> -> {blue, Xs}
		  end,
	Acc2 = Acc#{ Color => Count },
	case Rest2 of
		<<", ", Ys/binary>> -> parse_subset(Ys, Acc2);
		_ -> {Acc2, Rest2}
	end.

is_possible(Set) ->
	maps:get(red, Set, 0) =< 12
		andalso maps:get(green, Set, 0) =< 13
		andalso maps:get(blue, Set, 0) =< 14.

minimum_set(Sets) ->
	F = fun(_Color, Count1, Count2) -> max(Count1, Count2) end,
	lists:foldl(fun(X, Y) -> maps:merge_with(F, X, Y) end, #{}, Sets).

power(Set) -> maps:fold(fun(_Color, X, Y) -> X * Y end, 1, Set).

solve() ->
	{ok, Binary} = file:read_file("input"),
	Lines = binary:split(Binary, <<"\n">>, [trim, global]),
	Games = [parse_line(Line) || Line <- Lines],
	SolutionA = lists:sum([Id || {Id, Sets} <- Games, lists:all(fun is_possible/1, Sets)]),
	SolutionB = lists:sum([power(minimum_set(Sets)) || {_, Sets} <- Games]),
	{SolutionA, SolutionB}.
