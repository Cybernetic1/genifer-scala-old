package genifer3

// Store KB as a simple array, for now
class KB {
	val c = new Atom(1000)		// concept for "Cantonese"

	var kb: List[Formula] = List()

	def addFormula(f: Formula): Unit = {
		kb = f :: kb
	}
}

// Concepts dictionary: Int -> String
class dictionary {
	val dictMap = Map[Int, String](
		999  -> "->",
		1000 -> "CantoneseWord",
		1001 -> "CantonizeSentence",
		1002 -> "CantonizeWord"
	)
}
