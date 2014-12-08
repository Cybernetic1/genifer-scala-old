package genifer3;

// Term = list of Atoms
// Union = ∐ Terms              (disjoint union implemented as set)
// Formula = Term
//           | Union
//           | Formula → Formula
//           | ⋀ Formula        (probabilistic AND)

import java.util.ArrayList;
import java.util.Arrays;

class Term {
    public ArrayList<Atom> term;

    public Term() {
        term = null;
    }

    public Term(ArrayList<Atom> list) {
        term = list;
    }
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

    // Unify this.Formula with f
    public Boolean unify(Formula f) {
        return true;
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
