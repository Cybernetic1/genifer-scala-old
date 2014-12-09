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


// ******* Old stuff below ***********

// **** Older definition of Atoms
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
//class Formula(list: List[Atom]) {
//
//	// A Rule = Precond "->" Postcond
//	// How to represent a rule?
//
//	// var form: Set[List[Atom]] = Set(list)
//	val Formula: List[Atom] = list
//
//	def unify(f: Formula) : List[Atom] = {
//		Nil
//	}
//}
