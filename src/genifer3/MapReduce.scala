package genifer3

import genifer3.FormulaType._

// The essence of Map Reduce is to apply a set of operations to another set of objects.
// What we do here is to apply the set of Rules to the set of Facts.  This is similar to
// many rule-based systems such as Soar and JESS.

class MapReduce {

  // OUTPUT: a "mesh" which is a List of Formulas
  def map(rb: RulesBase, wmem: WorkingMemory): List[Formula] = {
    var mesh : List[Formula] = List()

    for (memItem <- wmem.mem) {
      for (rule <- rb.rules) {
        println("rule item = ", rule.toString)
        println("fact item = ", memItem.toString)

        // Here we may perform either matching or rewriting depending on KB item's type
        val result =
        if (rule.Type == Equation) {
          Rewriting.rewrite(memItem.asInstanceOf[⊙], rule.asInstanceOf[Equation])
        }
        else if (rule.Type == ⊙) {
          Matching.matching(memItem.asInstanceOf[⊙], rule.asInstanceOf[⊙])
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
    answer.asInstanceOf[⊙].atoms(0).str
  }

}
