package genifer3

// Keeps track of which concepts subsume (>) which others.
// For example:  animal > human > men > my friends > john

import scala.collection.mutable

object Ontology {

  val ontology = new mutable.HashMap[Int, mutable.Set[Int]] with mutable.MultiMap[Int, Int]

  // addBinding(node, parent)
  // 999 = concept of "everything"
  ontology.addBinding(1000, 999)
  ontology.addBinding(1001, 999)
  ontology.addBinding(1002, 999)
  ontology.addBinding(1003, 999)

  // ***** Determines if a concept subsumes another
  // OUTPUT: true if a1 ⊃ a2
  def ⊃(a1 : Atom, a2 : Atom) : Boolean = {
    // test if a1 is a2's parent
    ontology(a2.index)(a1.index)    // test for Set membership
  }
}
