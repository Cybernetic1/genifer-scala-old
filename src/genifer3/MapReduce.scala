package genifer3

class MapReduce {
  // Map should a "mesh" which is a List of Formulas
  def mapKB(msg: List[Formula]): List[Formula] = {
    List(new Formula(List(new Atom(Left(12)))))
  }

  // Reduce should return a Formula + Truth
  def reduce(mesh: List[Formula]): Formula = {
    new Formula(List(new Atom(Left(21))))
  }

  // Action should return a data item
  def action(command: Formula): String = {
    "Fine!"
  }

}
