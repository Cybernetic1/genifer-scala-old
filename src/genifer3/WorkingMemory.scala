package genifer3

// Latest idea is that all actions are performed in, and all rules are applied to,
// Working Memory instead of the KB.

class WorkingMemory {
  // Working Memory is a list with timed decay
  var mem: List[Formula] = List()

  def addFormula(f: Formula): Unit = {
    mem = f :: mem
  }


}
