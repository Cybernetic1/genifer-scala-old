package genifer3

class Subs {

}

class Sub {

}

class Unify {

  // ****** From Clojure, unify 2 Formulas that may contain unions
  //  (defn unify
  //    ([t1 t2]								; if called with default arguments
  //    (cond
  //      (and (set? t1) (set? t2))
  //  ;; If 2 sums unify, they must have the same number of terms
  //    (if (== (count t1) (count t2))
  //  ;; Try all combinations of sub-terms in t1 and t2, due to the commutativity of +
  //  ;; Collect and flatten all solutions
  //  (apply concat
  //    (for [s1 t1 s2 t2]
  //      (unify s1 s2)))
  //  false)
  //  (or (set? t1) (set? t2))
  //  ;; A sum is only unifiable with another sum
  //    false
  //    :else
  //  ;; Unify 2 plain terms (ie, pure compositions)
  //  (unify t1 t2 0 ())))

  // Main algorithm, will be explained in detail in the book:
  //  -- at any point, at most 1 variable on one side would be consuming input from the other side
  //  -- direction = which side has a consuming variable: 0 = none, + = left, - = right
  //  -- subs = the partial substitution of the consuming variable
  //  -- a substitution is a list (X,A,B,C...) representing { ABC... / X }
  // OUTPUT: a list of compound substitutions, each compound substitution is a set, ie, #{...}
  def unify(t1: Seq[Atom], t2: Seq[Atom], direction: Char, sub: Seq[Atom]): Option[Seq[Seq[Seq[Atom]]]] = {
    val a1 = t1.headOption
    val a2 = t2.headOption
    val r1 = t1.tail
    val r2 = t2.tail

    //  If either side is exhausted:
    if (a1.isEmpty && a2.isEmpty)
      Some(Seq(Seq(sub))) // success, regardless of direction
    else if (a1.isEmpty) {
      if (direction == '+')
        unify(t1, r2, '+', sub :+ a2.orNull) // :+ = append element
      else if (a2.orNull.isConst)
        None
      else addSub(sub, unify(t1, r2, '-', Seq(a2.orNull)))
    }
    else if (a2.isEmpty) {
      if (direction == '-')
        unify(r1, t2, '-', sub :+ a1.orNull)
      else if (a1.orNull.isConst)
         None
      else addSub(sub, unify(r1, t2, '+', Seq(a1.orNull)))
    }
    else {
      // continue to main course

      // at this point we know that neither sides is None
      val a1_ = a1.orNull
      val a2_ = a2.orNull
      direction match {
        // Fresh position: neither side is consuming
        case '0' =>
          if (a1_.isConst && a2_.isConst && testEq(a1_, a2_))
            unify(r1, r2, '0', Seq())
          else if (a1_.isConst && a2_.isConst && testNotEq(a1_, a2_))
            None
          else if (a1_.isVariable && a2_.isConst)
            forkSubs(
              addSub(Seq(a1_),
                unify(r1, t2, '0', Seq())),
              // Begin substitution { a2... / a1 }
              unify(r1, r2, '+', Seq(a2_, a1_)))
          else if (a1_.isConst && a2_.isVariable)
            forkSubs(
              addSub(Seq(a2_),
                unify(t1, r2, '0', Seq())),
              // Begin substitution { a1... / a2 }
              unify(r1, r2, '-', Seq(a1_, a2_)))
          else
          // Either a1 consumes a2 or vice versa
            forkSubs(unify(r1, r2, '+', Seq(a2_, a1_)),
              unify(r1, r2, '-', Seq(a1_, a2_)))

        // A variable (X) on the left is consuming
        //   1	(cond
        //      (and (const? a1) (const? a2) (test= a1 a2))
        //      (fork-subs
        //  ;; X ends and return to freshness
        //    (add-sub sub
        //      (unify r1 r2 0 ()))
        //  ;; X consumes a2
        //  (unify t1 r2 1 (cons a2 sub)))
        //  (and (const? a1) (const? a2) (test-not= a1 a2))
        //  ;; X must consume a2
        //    (unify t1 r2 1 (cons a2 sub))
        //  (and (const? a1) (variable? a2))
        //  (fork-subs
        //  ;; X consumes a2
        //  (unify t1 r2 1 (cons a2 sub))
        //  ;; X ends, a2 consumes a1
        //  (add-sub sub
        //    (unify r1 r2 -1 (list a1,a2))))
        //  (and (variable? a1) (const? a2))
        //  (fork-subs
        //  ;; X consumes a2
        //  (unify t1 r2 1 (cons a2 sub))
        //  ;; X ends, a1 consumes a2
        //  (add-sub sub
        //    (unify r1 r2 1 (list a2,a1))))
        //  :else
        //  ;; X consumes a2, OR a1 consumes a2, OR a2 consumes a1
        //    (fork-subs
        //      (unify t1 r2 1 (cons a2 sub))
        //  (fork-subs
        //    (add-sub sub
        //      (unify r1 r2 1 (list a2,a1)))
        //  (add-sub sub
        //    (unify r1 r2 -1 (list a1,a2))))))

        //  ;; A variable (Y) on the right is consuming -- mirrors case 1
        //  -1	(cond
        //    (and (const? a1) (const? a2) (test= a1 a2))
        //    (fork-subs
        //  ;; Y ends and return to freshness
        //    (add-sub sub
        //      (unify r1 r2 0 ()))
        //  ;; Y consumes a1
        //  (unify r1 t2 -1 (cons a1 sub)))
        //  (and (const? a1) (const? a2) (test-not= a1 a2))
        //  ;; Y must consume a1
        //    (unify r1 t2 -1 (cons a1 sub))
        //  (and (const? a1) (variable? a2))
        //  (fork-subs
        //  ;; Y consumes a1
        //  (unify r1 t2 -1 (cons a1 sub))
        //  ;; Y ends, a2 consumes a1
        //  (add-sub sub
        //    (unify r1 r2 -1 (list a1,a2))))
        //  (and (variable? a1) (const? a2))
        //  (fork-subs
        //  ;; Y consumes a1
        //  (unify r1 t2 -1 (cons a1 sub))
        //  ;; Y ends, a1 consumes a2
        //  (add-sub sub
        //    (unify r1 r2 1 (list a2,a1))))
        //  :else
        //  ;; Y consumes a1, a1 consumes a2, or a2 consumes a1
        //    (fork-subs
        //      (unify r1 t2 -1 (cons a1 sub))
        //      (fork-subs
        //        (add-sub sub
        //          (unify r1 r2 1 (list a2,a1)))
        //  (add-sub sub
        //    (unify r1 r2 -1 (list a1,a2))))))
        //  )))))
      }
    }
  }


  // Test equality of 2 Atoms
  // Both Atoms are known to be constants
  def testEq(x: Atom, y: Atom): Boolean = {
    if (x.index == 0 && y.index == 0)
      x.str == y.str
    else
      x.index == y.index
  }

  // Test inequality of 2 Atoms
  // Both Atoms are known to be constants
  def testNotEq(x: Atom, y: Atom): Boolean = {
    if (x.index == 0 && y.index == 0)
      x.str != y.str
    else
      x.index != y.index
  }

  // Merge 2 lists of compound substitutions
  // -- semantics is OR
  // -- INPUT: each of x, y is a list of compound subs
  // -- a compound sub is a set
  // -- OUTPUT: a list of compound subs
  def forkSubs(x: Option[Seq[Seq[Seq[Atom]]]], y: Option[Seq[Seq[Seq[Atom]]]]):
  Option[Seq[Seq[Seq[Atom]]]] = {
    if (x == None)
      y
    else if (y == None)
      x
    else
      Option(x.orNull ++ y.orNull)
  }

  // Add an atomic substitution to a list of compound substitutions
  // -- semantics is AND, distributed into the list
  // -- INPUT: x is an atomic sub, as a list (not set)
  //           y is a list of compound subs, ie, a list of sets
  //           On input, x needs to be reversed because it was built up via cons
  // -- OUTPUT: a list of compound subs
  def addSub(x: Seq[Atom], y: Option[Seq[Seq[Seq[Atom]]]]): Option[Seq[Seq[Seq[Atom]]]] = {
    // if (x == None)
    //   None
    // else if (y == None)
    //   None
    if (x.isEmpty)
      y
    else if (y.isEmpty)
      Option(Seq(Seq(x)))
    else
    //for each compound sub in y
      Option(y.orNull.map(y1 =>
        if (y1.head.isEmpty)
          Seq(x)
        else
          // do the set union
          // union(Seq(x), y1)
          Seq(x) ++ y1
      ))
  }

}