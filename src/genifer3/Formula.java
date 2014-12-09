package genifer3;

// Term = list of Atoms
// Union = ∐ Terms              (disjoint union implemented as set)
// Formula = Term
//           | Union
//           | Formula → Formula
//           | ⋀ Formula        (probabilistic AND)

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.RT;

class GeniferClojure {
    public static IFn unify;

    public void initClojure() {
        // System.out.println(System.getProperty("user.dir"));
        try {
            // This requires Genifer jar to be added to project
            RT.loadResourceScript("genifer/unify.clj");
        } catch (IOException e) {
            e.printStackTrace();
        }
        unify = Clojure.var("genifer.unification", "unify0");
        // System.out.println(unify.invoke("a", "a"));
        // Atom a = new Atom(1001);
        // System.out.println(Clojure.var("genifer.unification", "testing").invoke(a));
    }
}

class Term {
    public ArrayList<Atom> atoms;

    public Term() {
        atoms = null;
    }

    public Term(ArrayList<Atom> list) {
        atoms = list;
    }

    // Unify this.Term with t
    // OUTPUT: list of unifiers, each unifier is a pair
    public Object unify(Term t) {
        // System.out.println(this.atoms);
        // System.out.println(t.atoms);
        return GeniferClojure.unify.invoke(this.atoms, t.atoms);
    }

//    **** old code
//    public ArrayList<Pair<Atom, Atom>> unify2(Term t) {
//        if (isConst(this) || isConst(t)) {
//            if (this == t)
//                return new ArrayList();
//            else
//                return null;
//        }
//        if (isVar(this))
//            // occurs check?
//            // return new Pair<Atom, Atom>(t, this);
//        if (isVar(t))
//            // occurs check?
//            // return new Pair<Atom, Atom>(this, t);
//        if (this.atoms.isEmpty() || t.atoms.isEmpty())
//            return null;
//
//        while (true) {
//            Atom a1 = this.atoms.get(0);
//            Atom a2 = t.atoms.get(0);
//            // Pair subs1 = a1.unify(a2);
//            // Term tail1 = apply(subs1, this.atoms. (1, -1));
//            // Term tail2 = apply(subs1, t.atoms.subList(1, -1));
//            // ArrayList<Pair<Atom, Atom>> subs2 = tail1.unify(tail2);
//            // if (subs2 == null)
//                // return null;
//            // else
//                // return subs2; // .add(subs1);
//        }
//    }
//
//    public boolean isConst(Term t) {
//        return true;
//    }
//
//    public boolean isVar(Term t) {
//        return true;
//    }
}

class Union extends Term {
    public ArraySet union;

    public Union() {
        union = null;
    }

    public Union(Term... terms) {
        union = new ArraySet(5);
        for (Term term: terms)
            union.add(term);
    }

    public Union(ArrayList<Atom> list) {
        super(list);
    }
}

public class Formula extends Union {
    public Formula preCond;
    public Formula postCond;
    public Integer p_AND;

    public Formula(ArrayList<Atom> list) {
        super(list);
    }

}

// ********* Utility functions *********

// ******** Implementation of set as array
class ArraySet {
    private Term[] array;
    private int size = 0;

    public ArraySet(int capacity) {
        array = new Term[capacity];
        Arrays.fill(array, -1);
    }

    public boolean add(Term key) {
        int index = Arrays.binarySearch(array, 0, size, key);
        if (index < 0) {
            int insertIndex = -index-1;

            if(size < array.length - 1) {
                if(insertIndex < size) {
                    System.arraycopy(array, insertIndex, array, insertIndex + 1, size - insertIndex);
                }
                array[insertIndex] = key;
            } else {
                Term[] newArray = new Term[array.length + 1];
                System.arraycopy(array, 0, newArray, 0, insertIndex);
                System.arraycopy(array, insertIndex, newArray, insertIndex + 1, array.length - insertIndex);
                newArray[insertIndex] = key;
                array = newArray;
            }

            size++;
            return true;
        }
        return false;
    }

    public Term get(int position) {
        return array[position];
    }

    public int size() {
        return size;
    }

    public boolean contains(Term key) {
        return Arrays.binarySearch(array, key) >= 0;
    }
}
