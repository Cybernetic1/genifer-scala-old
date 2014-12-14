package genifer3

class MapReduce {

  // OUTPUT: a "mesh" which is a List of Formulas
  def map(kb: KB, jug: List[Formula]): List[Formula] = {
    val u = new Unify()
    var mesh : List[Formula] = List()

    for (kbItem <- kb.kb) {
      for (jugItem <- jug) {
        println("jug item = ", jugItem.toString)
        println("KB item = ", kbItem.toString)
        val result = u.unify(jugItem, kbItem, '0', new u.Sub())
        println("result:")
        println(result.getOrElse("false").toString)
        println("\n********\n")

        // if match, collect matched results
        if (result.nonEmpty)
          mesh ::= kbItem
      }
    }
    mesh
  }

  // **** Reduce: from a mesh of relations find the truth of the goal
  // OUTPUT: a Formula + Truth
  def reduce(mesh: List[Formula]): Formula = {
    mesh.head   // for now, the mesh returns the single result
  }

  // **** Perform an action
  // OUTPUT: a data item
  def action(command: Formula): String = {
    command.atoms(1).str
  }

}
