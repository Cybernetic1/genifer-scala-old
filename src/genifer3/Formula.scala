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

  // def unify(t: ∏): Subs = {
  //   val result: Subs = null
  //   result
  // }

  def headOption : Option[Atom] = {
    atoms.headOption
  }

  def isEmpty : Boolean = {
    atoms.isEmpty
  }

  def tail : ∏ = {
    val result : ∏ = new ∏()
    result.atoms = atoms.tail
    result
  }

  override def toString : String = {
    var str : String = ""
    for (a <- atoms)
      if (a.index == 0)
        str += a.str + ", "
      else
        str += a.index.toString + ", "
    str
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
