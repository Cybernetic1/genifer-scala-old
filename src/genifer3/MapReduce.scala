package genifer3

class MapReduce {

  // OUTPUT: a "mesh" which is a List of Formulas
  def map(kb: KB, jug: List[Formula]): List[Formula] = {
    for (kbItem <- kb.kb) {
      for (jugItem <- jug) {
        val result = jugItem.unify(kbItem)
        println("\n********\n")
        // if match, collect matched results
      }
    }
    List()
  }

  // OUTPUT: a Formula + Truth
  def reduce(mesh: List[Formula]): Formula = {
    new Formula()
  }

  // **** Perform an action
  // OUTPUT: a data item
  def action(command: Formula): String = {
    "Fine!"
  }

}
