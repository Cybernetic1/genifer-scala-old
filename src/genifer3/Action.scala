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

object Action {
  // **** Perform an act
  // An act is a logic formula, beginning with an action atom
  // Available actions:
  // * focus        (on tape1)
  // * focus_next   (on tape1)
  // * append       item (to tape2)
  // * extract      (from KB to tape2)
  def perform(act : ⊙) = {
    val action : Atom = act.atoms(0)    // first Atom is the action type
    action.index match {

      case 100 =>                       // 100 = focus
        // Atom arg = act.atoms(1)      // argument = which list to focus
        Tapes.focus1 = 0
        // "Manifest" the item under focus
        focus(null)                     // null = default to list1

      case 101 =>                       // 101 = move focus next
        Tapes.focus1 += 1
        focus(null)

      case 102 =>                       // 102 = append
        // Default to tape2
        val arg = act.atoms(1)
        Tapes.tape2 :+ arg

      case 103 =>                       // 103 = extract to out-tape
        // tape2 = out-tape
        val arg = act.atoms(1)
        extract(arg)

      case _ =>
        null
    }
  }

  // **** Focus on list location
  // Express / manifest the list item that is currently under focus
  // In other words, express as logical facts, output to KB
  def focus(whichList : Atom) = {
    // express facts about the list item under focus
    val a0 : Atom = Tapes.tape1(Tapes.focus1)

    // Output the single-atom formula to KB
    val f = new ⊙
    f.atoms = Seq(a0)
    Genifer3.wmem.addFormula(f)

    // If we can look-ahead 1 atom, write formula [a0, a1] to KB
    if (Tapes.focus1 < Tapes.tape1.length) {
      val a1: Atom = Tapes.tape1(Tapes.focus1 + 1)
      val f = new ⊙
      f.atoms = Seq(a0, a1)
      Genifer3.wmem.addFormula(f)
    }
  }

  // **** Extract an item from Working Memory, given a superclass.
  // The extracted result should be stored on tape, which would be an atom.
  // So the question is how to extract one atom from a formula.
  // Perhaps if a formula is a sequence of conjunctions we simply pick the last atom
  // in the sequence that is subsumed by the superclass?  But this may get ambiguous.
  // At this point, just assume the facts are single-atom.  At a later stage, we can
  // use multi-atom superclasses to extract facts.
  def extract(superclass : Atom) = {
    var result : Option[Atom] = null

    for (item <- Genifer3.wmem.mem) {
      val head = item.asInstanceOf[⊙].atoms(0)
      if (⊃(superclass, head))
        result = Some(head)
    }
    // The extracted atom should go to the output tape.
    // Later we may also have variations of this.
    Tapes.tape2 :+ result
  }

}
