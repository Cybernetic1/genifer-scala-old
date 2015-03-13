package genifer3

// See the file RulesBase.scala for an explanation of the distinction
// between Facts and Rules.

class FactsBase {
  var facts: List[Formula] = List()

  def addFormula(f: Formula): Unit = {
    facts = f :: facts
  }

}
