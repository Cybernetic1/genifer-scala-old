package genifer3

// The idea of Tapes is from Turing machines, but the best way to think of Tapes
// is as CPU registers as opposed to RAM.
//
// One difference is that each tape position can store a logic formula.

object Tapes {
  // tapes are implemented as simple lists
  var tape1 : Seq[Atom] = null      // input tape
  var tape2 : Seq[Atom] = null      // output tape
  var tape3 : Seq[⊙] = null         // program tape

  // The "focus" is an index over the tape, denoting the point of focus
  var focus1 : Integer = 0

  // Program counter for the Program Tape
  var progCounter : Integer = 0

  // Initialize input sequence (just an example)
  tape1 = Seq(new Atom("做"), new Atom("什"), new Atom("么"))

  // Initialize program tape
  tape3 = Seq(new ⊙(List(new Atom(100))))
}
