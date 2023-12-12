# AOC 2023 day 11 in Dafny

method {:extern "Day11", "ReadFileToString"} ReadFileToString(name: string)
	returns (s: string)

method lines(s: string)
	returns (lines: seq<string>) {
	lines := [];
	var index := 0;
	var start := 0;
	while index < |s|
		invariant start <= index {
		if s[index] == '\n' {
			lines := lines + [s[start..index]];
			start := index + 1;
		}
		index := index + 1;
	}
	if start < |s| { lines := lines + [s[start..]]; }
}

predicate Grid(ls: seq<string>) { forall i :: 1 <= i < |ls| ==> |ls[i]| == |ls[0]| }

function stars(ls: seq<string>): set<(int, int)> {
	set y, x | 0 <= y < |ls| && 0 <= x < |ls[y]| && ls[y][x] == '#' :: (y, x)
}

method computeLengths(ls: seq<string>, emptyRows: array<bool>, emptyColumns: array<bool>)
	returns (totalLength: int)
	requires Grid(ls)
	requires |ls| > 0
	requires emptyRows.Length == |ls| requires emptyColumns.Length == |ls[0]| {
	var stars := stars(ls);
	// var expansion := 1; // Part 1
	var expansion := 1000000 - 1; // Part 2

	totalLength := 0;
	var xs := stars;
	while xs != {} {
		var a :| a in xs;
		xs := xs - { a };

		var ys := xs;
		while ys != {} {
			var b :| b in ys;
			ys := ys - { b };

			var yMin, yMax := a.0, b.0;
			if yMin > yMax { yMin, yMax := yMax, yMin; }
			var xMin, xMax := a.1, b.1;
			if xMin > xMax { xMin, xMax := xMax, xMin; }
			totalLength := totalLength + (yMax - yMin) + (xMax - xMin);

			var y := yMin;
			while y < yMax {
				if emptyRows[y] { totalLength := totalLength + expansion; }
				y := y + 1;
			}

			var x := xMin;
			while x < xMax {
				if emptyColumns[x] { totalLength := totalLength + expansion; }
				x := x + 1;
			}
		}
	}
}

method {:main} Main() {
	var s := ReadFileToString("input");
	var ls := lines(s);
	expect Grid(ls), "grid is regular";
	expect |ls| > 0 && |ls[0]| > 0, "grid is nonempty";

	var rows, cols := |ls|, |ls[0]|;

	var emptyRows := new bool[rows](y => 0 <= y < rows
		&& forall x :: 0 <= x < cols ==> ls[y][x] == '.');
	var emptyColumns := new bool[cols](x => 0 <= x < cols
		&& forall y :: 0 <= y < rows ==> ls[y][x] == '.');

	var totalLengths := computeLengths(ls, emptyRows, emptyColumns);
	print "Total lengths: ", totalLengths, "\n";
}
