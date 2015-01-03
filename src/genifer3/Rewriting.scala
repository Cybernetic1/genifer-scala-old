package genifer3

// An equation expresses that one thing "is" another thing.
// Equations define the rewriting system.
// Rewriting is a sub-system of the logic and we store equations separately.

// Equations are "oriented" in the sense that we are happy to rewrite
// from left -> right but not the other direction.

// An equation defines "left-hand-side = right-hand-side"
class Equation {
  var left: ∏ = null
  var right: ∏ = null

  def this(t1: ∏, t2: ∏) {
    this()
    this.left = t1
    this.right = t2
  }
}

class Rewriting {
  var equations: List[Equation] = List()

  def addEquation(t1: ∏, t2: ∏): Unit = {
    val eq = new Equation(t1, t2)
    equations = eq :: equations
  }

  // Given a term t, rewrite it to its normal form n.
  // Do we need to consider subsumption?
  def rewrite(t: ∏): ∏ = {
    val result: ∏ = new ∏()
    for (eq <- equations) {
      // match partially
      for (i <- 0 to t.atoms.length - eq.left.atoms.length) {
        var j: Int = i
        for (a <- eq.left.atoms) {
          if (a == t.atoms(j)) {
            result.atoms +:= eq.right
          }
          else
            result.atoms +:= t.atoms(j)
          j += 1
        }
      }
    }
    result
  }

}
