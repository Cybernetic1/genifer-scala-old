package genifer3

// Term  = ∏ Atoms              (Term = list of Atoms)
// Union = ∐ ∏                  (disjoint union of Terms)
// Formula = ∏ | ∐
//           | Formula → Formula
//           | ⋀ Formulas       (probabilistic AND)

// ∏ = Term = list of Atoms
class ∏  {
  var atoms: Seq[Atom] = null

  def this(list: Seq[Atom]) {
    this()
    this.atoms = list
  }

  def unify(t: ∏): Subs = {
    val result: Subs = null
    result
  }
}

// ∐ = disjoint union of Terms
class ∐ extends ∏ {
  var union: Seq[∏] = null

  // Union of multiple terms
  def this(terms: ∏ *) {
    this()
    for (term <- terms)
      union.+:(term)
  }
}

class Formula extends ∐ {
  var preCond : Formula = null
  var postCond : Formula = null
  var ⋀ : Integer = 0

  def this(list: Seq[Atom]) {
    this()
    new ∏ (list)
  }
}
