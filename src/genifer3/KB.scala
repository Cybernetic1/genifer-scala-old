package genifer3

// To-do:
// -- read file and add formulas for Cantonese
// -- set up jar for matching
// -- after matching construct answer
// -- code to execute actions

// An atoms is either a concept (represented by its index)
// or a string datum
class Atom(x: Either[Int, String]) {
	var datum = x match {
		case Left(int) => int
		case Right(str) => str
	}
}

// Define Formula as set of lists
class Formula(l: List[Atom]) {
	var form: Set[List[Atom]] = Set(l)
}

// Define KB as array
class KB {
	val c = new Atom(Left(1000))		// concept for "Cantonese"

	var kb: List[Formula] = List(
		new Formula(List(c, new Atom(Right("shit")))),
		new Formula(List(c, new Atom(Left(5))))
	)

	def addFormula(f: Formula): Unit = {
		kb = f :: kb
	}
}

// Define concepts dictionary: Int -> String
class dictionary {
	val dictMap = Map[Int, String](
		1000 -> "Cantonese"			// this is a user-defined concept
	)
}


