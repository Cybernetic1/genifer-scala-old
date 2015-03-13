package genifer3

// A Fact is a simple proposition.
// A Rule contains 2 parts:  precondition and postcondition.
// Therefore, Rules can be applied to Facts to yield new Facts.

// Rules include Conditionals and Equations, with the = sign taken as uni-directional.

// The iteration of inference goes as follows:
//      WM'   = RB∙( FB + WM )
//      WM''  = RB∙( FB + WM' )
//      WM''' = ...
//      WM*   = RB( FB + _ )^∞ ∙ WM
// where  WM = Working Memory
//        RB = Rules Base
//        FB = Facts Base
//        ∙ means application
//        WM* = final Facts after infinite iterations
// In other words, the set of Rules is applied on the Working Memory
// together with Facts in the FactsBase.

class RulesBase {
  var rules: List[Formula] = List()

  def addFormula(f: Formula): Unit = {
    rules = f :: rules
  }

}
