package genifer3

class Unify {

  // For example, unify( X loves Y, john loves mary)
  // will return [{[john / X], [mary / Y}] as one solution,
  // {} denotes sets and [] denotes lists
  // In other words, the result is a list of sets of lists.

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

  // Main algorithm
  // The main idea is to "eat" (consume) the formulas from their heads,
  // like 2 Pacmans:
  //      ᗧ X loves Y       ᗧ john loves mary
  // and compare the heads and decide what to do.
  // The rest is pretty straightforward.
  //  -- at any point, at most 1 variable on one side would be consuming input from the other side
  //  -- direction = which side has a consuming variable: 0 = none, + = left, - = right
  //  -- sub = the partial substitution of the consuming variable
  //  -- a substitution is a list (X,A,B,C...) representing { ABC... / X }
  // OUTPUT: a list of compound substitutions, each compound substitution is a set, ie, #{...}
  def unify(t1: ∏, t2: ∏, direction: Char, sub: Sub): Option[Subs] = {
    val a1 = t1.headOption
    val a2 = t2.headOption
    val r1 = if (t1.isEmpty) null else t1.tail
    val r2 = if (t2.isEmpty) null else t2.tail

    //  If either side is exhausted:
    if (a1.isEmpty && a2.isEmpty)
      Some(new Subs(sub))                    // success, regardless of direction
    else if (a1.isEmpty) {
      if (direction == '+')
        unify(t1, r2, '+', sub :+ a2.orNull) // :+ = append element
      else if (a2.orNull.isConst)
        None
      else addSub(sub, unify(t1, r2, '-', new Sub(a2.orNull)))
    }
    else if (a2.isEmpty) {
      if (direction == '-')
        unify(r1, t2, '-', sub :+ a1.orNull)
      else if (a1.orNull.isConst)
         None
      else addSub(sub, unify(r1, t2, '+', new Sub(a1.orNull)))
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
            unify(r1, r2, '0', new Sub())
          else if (a1_.isConst && a2_.isConst && testNotEq(a1_, a2_))
            None
          else if (a1_.isVariable && a2_.isConst)
            forkSubs(
              addSub(new Sub(a1_),
                unify(r1, t2, '0', new Sub())),
              // Begin substitution { a2... / a1 }
              unify(r1, r2, '+', new Sub(a2_, a1_)))
          else if (a1_.isConst && a2_.isVariable)
            forkSubs(
              addSub(new Sub(a2_),
                unify(t1, r2, '0', new Sub())),
              // Begin substitution { a1... / a2 }
              unify(r1, r2, '-', new Sub(a1_, a2_)))
          else
          // Either a1 consumes a2 or vice versa
            forkSubs(unify(r1, r2, '+', new Sub(a2_, a1_)),
              unify(r1, r2, '-', new Sub(a1_, a2_)))

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

  // Represents a single substitution {a b c... / x}
  class Sub {
    var atoms : Seq[Atom] = null

    def this(s : Seq[Atom]) {
      this()
      this.atoms = s
    }

    def this(a : Atom) {
      this()
      this.atoms = Seq(a)
    }

    def this(a1 : Atom, a2 : Atom) {
      this()
      this.atoms = Seq(a1, a2)
    }

    def :+ (a : Atom): Sub = {
      this.atoms :+ a
      this
    }

    def isEmpty: Boolean = {
      atoms.isEmpty
    }
  }

  // List of compound subs
  // Each compound Sub = a Set of Subs
  class Subs {
    var subs : Seq[Set[Sub]] = null

    // Create compound sub from a single sub
    def this(sub : Sub) {
      this()
      this.subs = Seq(Set(sub))
    }

    def isEmpty : Boolean = {
      subs.isEmpty
    }
  }

  // The idea of forkSubs is that 2 possible branches of substitutions
  // are possible.  So we will merge 2 lists of compound substitutions
  // as answers.
  // -- semantics is OR
  // -- INPUT: each of x, y is a list of compound subs
  // -- a compound sub is a set
  // -- OUTPUT: a combined list of compound subs
  def forkSubs(x: Option[Subs], y: Option[Subs]): Option[Subs] = {
    if (x == None)
      y
    else if (y == None)
      x
    else {
      val s = new Subs()
      s.subs = x.orNull.subs ++ y.orNull.subs
      Some(s)
    }
  }

  // Add an atomic substitution to a list of compound substitutions
  // -- semantics is AND, distribute into the list
  // -- INPUT: x is an atomic sub, as a list (not set)
  //           y is a list of compound subs, ie, a list of sets
  // -- OUTPUT: a list of compound subs
  def addSub(x: Sub, y: Option[Subs]): Option[Subs] = {
    // if (x == None)
    //   None
    // else if (y == None)
    //   None
    if (x.isEmpty)
      y
    else if (y.isEmpty)
      Some(new Subs(x))
    else {
      val s = new Subs()
      //for each compound sub in y
      s.subs = y.orNull.subs.map(y1 =>
        if (y1.head.isEmpty)
          Set(x)
        else
        // do the set union
        // union(Set(x), y1)
          Set(x) ++ y1
      )
      Some(s)
    }
  }

}