package genifer3

// An Atom is the most basic unit in Genifer logic.
// An Atom represents a basic concept.
// It is simply represented by an index.

// We also allow special Atoms that has no index (ie index = 0)
// to hold data items as Strings.
// Such Atoms are not concepts and they have no "place" in conceptual space.

// If the index is negative, it represents a Variable.
// Variables are also not concepts.

class Atom {
  var index: Int = 0
  var str: String = null

  // Creates an ordinary Atom (ie, a concept)
  def this(i: Int) {
    this()
    this.index = i
    this.str = null
  }

  // Creates a data Atom which is not a concept
  def this(s: String) {
    this()
    this.index = 0
    this.str = s
  }
}
