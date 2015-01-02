package genifer3

class MapReduce {

  // OUTPUT: a "mesh" which is a List of Formulas
  def map(kb: KB, jug: List[Formula]): List[Formula] = {
    var mesh : List[Formula] = List()

    for (kbItem <- kb.kb) {
      for (jugItem <- jug) {
        println("jug item = ", jugItem.toString)
        println("KB item = ", kbItem.toString)
        val result = Matching.matching(jugItem, kbItem)
        println("result:")
        println(result.toString)
        println("\n********\n")

        // if match, collect matched results
        if (result)
          mesh ::= kbItem
      }
    }
    mesh
  }

  // **** Reduce: from a mesh of relations find the truth of the goal
  // OUTPUT: a Formula + Truth
  def reduce(mesh: List[Formula]): Formula = {
    if (mesh.nonEmpty)
      mesh.head   // for now, the mesh returns the single result
    else
      null
  }

  // **** Perform an action
  // OUTPUT: a data item
  def action(command: Formula): String = {
    command.atoms(1).str
  }

}
