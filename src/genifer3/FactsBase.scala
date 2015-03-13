package genifer3

class FactsBase {
  var facts: List[Formula] = List()

  def addFormula(f: Formula): Unit = {
    facts = f :: facts
  }

}
