package genifer3

// To-do:
// - create tree / forest data structure
// - offer elementary operations:
//    - add node at current level
//    - move up to parent
//    - move down to left-most child
//    - move down to right-most child
//    - move down to random child
//    - focus on list of children

// - create list data structure
// - offer elementary operations:
//    - focus on list head (1st element)
//    - focus on next element
//    - "fuzzy" match focus with X

import genifer3.Ontology.⊃

class Action {
  // tapes are implemented as simple lists
  var tape1 : Seq[Atom] = null     // input list
  var tape2 : Seq[Atom] = null     // output list

  var ans0 : Atom = null      // last answer
  var ans1 : Atom = null      // second last answer
  var ans2 : Atom = null      // third last answer

  // The "focus" is the index into a list, denoting the point of focus
  var focus1 : Integer = 0
  // var focus2 : Integer = 0

  // Initialize input sequence (just an example)
  tape1 = Seq(new Atom("做"), new Atom("什"), new Atom("么"))

  // **** Perform an act
  // An act is a logic formula, beginning with an action atom
  // Available actions:
  // * focus        list1/list2
  // * focus_next   list1/list2
  // * append       list1/list2, item
  def perform(act : ∏) = {
    val action : Atom = act.atoms(0)    // first Atom is the action type
    action.index match {

      case 100 =>                       // 100 = focus
        // Atom arg = act.atoms(1)      // argument = which list to focus
        focus1 = 0
        // "Manifest" the item under focus
        focus(null)                     // null = default to list1

      case 101 =>                       // 101 = move focus next
        focus1 += 1
        focus(null)

      case 102 =>                       // 102 = append
        // Default to list2
        val arg = act.atoms(1)
        tape2 :+ arg

      case _ =>
        null
    }
  }

  // **** Focus on list location
  // Express / manifest the list item that is currently under focus
  // In other words, express as logical facts, output to KB
  def focus(whichList : Atom) = {
    // express facts about the list item under focus
    val a0 : Atom = tape1(focus1)

    // Output the single-atom formula to KB
    val f = new ∏
    f.atoms = Seq(a0)
    Genifer3.wmem.addFormula(f)

    // If we can look-ahead 1 atom, write formula [a0, a1] to KB
    if (focus1 < tape1.length) {
      val a1: Atom = tape1(focus1 + 1)
      val f = new ∏
      f.atoms = Seq(a0, a1)
      Genifer3.wmem.addFormula(f)
    }
  }

  // **** Extract an item from Working Memory, given a superclass.
  // The extracted result should be stored on tape, which would be an atom.
  // So the question is how to extract one atom from a formula.
  // Perhaps if a formula is a sequence of conjunctions we simply pick the last atom
  // in the sequence that is subsumed by the superclass?  But this may be ambiguous.
  // At this point, just assume the facts are single-atom.  At a later stage, we can
  // use multi-atom superclasses to extract facts.
  def extract(superclass : Atom) = {
    var result = Genifer3.nullAtom

    for (item <- Genifer3.wmem.mem) {
      val head = item.asInstanceOf[∏].atoms(0)
      if (⊃(superclass, head))
        result = head
    }
    result
  }

}
