package genifer3

import genifer3.FormulaType._

class MapReduce {

  // OUTPUT: a "mesh" which is a List of Formulas
  def map(kb: KB, jug: List[Formula]): List[Formula] = {
    var mesh : List[Formula] = List()

    for (kbItem <- kb.kb) {
      for (jugItem <- jug) {
        println("jug item = ", jugItem.toString)
        println("KB item = ", kbItem.toString)

        // Here we may perform either matching or rewriting depending on KB item's type
        val result =
        if (kbItem.Type == Equation) {
          Rewriting.rewrite(jugItem.asInstanceOf[∏], kbItem.asInstanceOf[Equation])
        }
        else if (kbItem.Type == ∏) {
          Matching.matching(jugItem.asInstanceOf[∏], kbItem.asInstanceOf[∏])
        }
        println("result:")
        if (result != null)
          println(result.toString)
        println("\n********\n")

        // if match, collect matched results
        if (result != null)
          mesh ::= result.asInstanceOf[Formula]
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
  def action(answer: Formula): String = {
    answer.asInstanceOf[∏].atoms(0).str
  }

}
