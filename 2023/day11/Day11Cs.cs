using System.IO;

public partial class Day11 {
	public static Dafny.ISequence<Dafny.Rune> ReadFileToString(Dafny.ISequence<Dafny.Rune> name) {
		return Dafny.Sequence<Dafny.Rune>.UnicodeFromString(File.ReadAllText(name.ToVerbatimString(false)));
	}
}
