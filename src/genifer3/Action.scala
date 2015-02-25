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


class Action {
  // currently Working Memory is implemented as a simple list
  var workingMemory1 : Seq[Atom] = null     // input list
  var workingMemory2 : Seq[Atom] = null     // output list

  // The "focus" is the index into a list, denoting the point of focus
  var focus1 : Integer = 0
  // var focus2 : Integer = 0

  // Initialize input sequence (just an example)
  workingMemory1 = Seq(new Atom("做"), new Atom("什"), new Atom("么"))

  // **** Perform an act
  // An act is a logic formula
  // Available actions:
  // * focus        list1/list2
  // * focus_next   list1/list2
  def perform(act : ∏) = {
    val action : Atom = act.atoms(0)    // first Atom is the action type
    action.index match {

      case 100 =>                       // 100 = focus
        // Atom arg = act.atoms(1)      // argument = which list to focus
        focus1 = 0
        // "Manifest" the item under focus
        focus(null)                     // null = default to list1

      case 101 =>                       // 101 = focus next
        focus1 += 1
        focus(null)

      case 102 =>                       // 102 = append
        val arg = act.atoms(1)
        workingMemory2 :+ arg

      case _ =>
        null
    }
  }

  // **** Focus on list location
  // Express / manifest the list item that is currently under focus
  // In other words, express as logical facts, output to KB
  def focus(whichList : Atom) = {
    // express facts about the list item under focus
    val a0 : Atom = workingMemory1(focus1)

    // Output the single-atom formula to KB
    val f = new ∏
    f.atoms = Seq(a0)
    Genifer3.kb.addFormula(f)

    // If we can look-ahead 1 atom, write formula [a0, a1] to KB
    if (focus1 < workingMemory1.length) {
      val a1: Atom = workingMemory1(focus1 + 1)
      val f = new ∏
      f.atoms = Seq(a0, a1)
      Genifer3.kb.addFormula(f)
    }
  }
}
