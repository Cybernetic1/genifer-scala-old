package genifer3

class RulesBase {
  var rules: List[Formula] = List()

  def addFormula(f: Formula): Unit = {
    rules = f :: rules
  }

}
