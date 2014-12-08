package genifer3;

// An Atom is the most basic unit in Genifer logic.
// It is simply represented by an index.

// However, we allow special Atoms that has no index (ie index = 0)
// to hold data items as Strings.
// Such Atoms are not concepts and they have no "place" in conceptual space.

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
}
