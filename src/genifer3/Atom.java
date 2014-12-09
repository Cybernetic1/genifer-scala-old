package genifer3;

// An Atom is the most basic unit in Genifer logic.
// An Atom represents a basic concept.
// It is simply represented by an index.

// We also allow special Atoms that has no index (ie index = 0)
// to hold data items as Strings.
// Such Atoms are not concepts and they have no "place" in conceptual space.

// If the index is negative, it represents a Variable.
// Variables are also not concepts.

public class Atom {
    public final int index;
    public final String str;

    // Creates an ordinary Atom (ie, a concept)
    public Atom(int i) {
        this.index = i;
        this.str = null;
    }

    // Creates a data Atom which is not
    public Atom(String s) {
        this.index = 0;
        this.str = s;
    }

    // Unify 2 Atoms
    // OUTPUT: a list of pairs where the 2nd element is always a variable
//    public Pair<Atom, Atom> unify(Atom a) {
//        if (this.index > 0 && a.index > 0) {
//            if (this.index == a.index)
//                return new Pair<Atom, Atom>(null, null);
//            else
//                return null;
//        }
//        if (this.index < 0)
//            if
//    }
}
