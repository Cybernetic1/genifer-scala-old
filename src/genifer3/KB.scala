package genifer3

// Store KB as a simple array, for now
class KB {
	var kb: List[Formula] = List()

	def addFormula(f: Formula): Unit = {
		kb = f :: kb
	}
}

// Concepts dictionary: Int -> String
class dictionary {
	val dictMap = Map[Int, String](

    // Actions
    100   -> "focus",
    101   -> "focus-next",
    102   -> "append",

    // General concepts
		998   -> "->",			// implication arrow, not sure if useful here
		999   -> "Everything",
		1000  -> "CantoneseWord",
		1001  -> "CantonizeSentence",
		1002  -> "CantonizeWord"

	)
}
