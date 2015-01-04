package genifer3

// An equation expresses that one thing "is" another thing.
// Equations define the rewriting system.
// Rewriting is a sub-system of the logic and we store equations separately.

// Equations are "oriented" in the sense that we are happy to rewrite
// from left -> right but not the other direction.

object Rewriting {

  // Given a term t, try to rewrite it using a given rewrite rule (equation)
  // Do we need to consider subsumption?
  def rewrite(t: ∏, eq: Equation): ∏ = {
    val result: ∏ = new ∏
    var mismatch : Boolean = false
    // match partially
    for (i <- 0 to t.atoms.length - eq.left.atoms.length) {
      mismatch = false
      var j : Int = 0
      for (a <- eq.left.atoms) {
        if (!(a == t.atoms(i + j)))
          mismatch = true
        j += 1
      }
      if (!mismatch)
        result.atoms = t.atoms.take(i) ++ eq.right.atoms ++ t.atoms.drop(i + eq.left.atoms.length)
    }
    if (!mismatch)
      result
    else
      null
  }

}
