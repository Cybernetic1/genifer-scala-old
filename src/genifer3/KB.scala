package genifer3

// An atoms is either a concept (represented by its index)
// or a string datum
//class Atom(x: Either[Int, String]) {
//	val datum = x match {
//		case Left(int) => int
//		case Right(str) => str
//	}
//}

// A Formula is a Set of Lists of Atoms (ie, union of products)
// but at this stage, we skip the union (Set)
// and only implment Formula = List of Atoms
class Formula(list: List[Atom]) {

	// A Rule = Precond "->" Postcond
	// How to represent a rule?

	// var form: Set[List[Atom]] = Set(l)
	val Formula: List[Atom] = list

	def unify(f: Formula) : List[Atom] = {
		Nil
	}
}


// Define KB as array
class KB {
	val c = new Atom(1000)		// concept for "Cantonese"

	var kb: List[Formula] = List(
		new Formula(List(c, new Atom("shit"))),
		new Formula(List(c, new Atom(5)))
	)

	def addFormula(f: Formula): Unit = {
		kb = f :: kb
	}
}

// Define concepts dictionary: Int -> String
class dictionary {
	val dictMap = Map[Int, String](
		999  -> "->",
		1000 -> "CantoneseWord",
		1001 -> "CantonizeSentence",
		1002 -> "CantonizeWord"
	)
}
