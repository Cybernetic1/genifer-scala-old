package genifer3

import java.util

class MapReduce {

  // OUTPUT: a "mesh" which is a List of Formulas
  def map(kb: KB, jug: List[Formula]): util.LinkedList[Formula] = {
    val u = new Unify()
    for (kbItem <- kb.kb) {
      for (jugItem <- jug) {
        println("jug item atoms: ", jugItem.atoms)
        println("KB item atoms: ", kbItem.atoms)
        val result = u.unify(jugItem, kbItem, '0', new u.Sub())
        println("\n********\n")
        // if match, collect matched results
      }
    }
    new util.LinkedList()
  }

  // OUTPUT: a Formula + Truth
  def reduce(mesh: util.LinkedList[Formula]): Formula = {
    new Formula()
  }

  // **** Perform an action
  // OUTPUT: a data item
  def action(command: Formula): String = {
    "Fine!"
  }

}
