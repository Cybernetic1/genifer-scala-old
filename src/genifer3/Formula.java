package genifer3;

// A Genifer formula is a disjoint union of products (sum-of-products)

// As to implementation, a Formula is a Set of Lists.

// A Fact is a simple Formula
// A Rule is a Formula with Pre-cond and Post-cond

import java.util.ArrayList;

public class Formula {
    public ArrayList<Atom> preCond;
    public ArrayList<Atom> postCond;

    public Formula(ArrayList<Atom> list) {

    }

    // Unify this.Formula with f
    public Boolean unify(Formula f) {
        return true;
    }
}
