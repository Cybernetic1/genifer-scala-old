package genifer3

// Term  = ∏ Atoms              (∏ = Term = list of Atoms)
// Union = ∐ ∏                  (∐ = disjoint union of Terms)
// Formula = ∏ | ∐
//           | Term = Term             (equation)
//           | ⋀ Formulas → Formula    (probabilistic conditional)

object FormulaType extends Enumeration {
  val ∏, ∐, Equation, Conditional = Value
  // type FormulaType = Value
}

abstract class Formula {
  val Type : FormulaType.Value
}

// ∏ = Term = list of Atoms
class ∏ extends Formula {
  val Type = FormulaType.∏
  var atoms: Seq[Atom] = null

  def this(list: Seq[Atom]) {
    this()
    this.atoms = list
  }

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
class ∐ extends Formula {
  val Type = FormulaType.∐
  var union: Seq[∏] = null

  // Union of multiple terms
  def this(terms: ∏ *) {
    this()
    for (term <- terms)
      union.+:(term)
  }
}

class Equation extends Formula {
  val Type = FormulaType.Equation

  var left: ∏ = null
  var right: ∏ = null

  def this(left: Seq[Atom], right: Seq[Atom]) {
    this()
    this.left = new ∏(left)
    this.right = new ∏(right)
  }

  def this(t1: ∏, t2: ∏) {
    this()
    this.left = t1
    this.right = t2
  }

  override def toString : String = {
    this.left.toString + " = " + this.right.toString
  }
}

class Conditional extends Formula {
  val Type = FormulaType.Conditional

  var preCond: Formula = null
  var postCond: Formula = null
  var ⋀ : List[Int] = null     // These should be parameters, to be elaborated later

  // *** methods incomplete...

  def this(list: Seq[Atom]) {
    this()
    new ∏(list)
  }
}
