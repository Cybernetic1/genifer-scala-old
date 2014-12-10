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
  //  -- direction = which side has a consuming variable: 0 = none, 1 = left, -1 = right
  //  -- sub = the partial substitution of the consuming variable
  //  -- a substitution is a list (X,A,B,C...) representing { ABC... / X }
  // OUTPUT: a list of compound substitutions, each compound substitution is a set, ie, #{...}
  def unify(t1: ⦿, t2: ⦿, direction: Int, subs: Subs): Subs = {
    val a1 = t1.atoms.head
    val a2 = t2.atoms.head
    val r1 = t1.atoms.tail
    val r2 = t2.atoms.tail

    // if (a1)
    //  ;; If either side is exhausted:
    //    (cond
    //  (and (nil? a1) (nil? a2))
    //  (list #{(reverse sub)})		; success, regardless of direction
    //  (nil? a1)
    //  (if (== direction 1)
    //    (unify t1 r2 1 (cons a2 sub))
    //  (if (const? a2)
    //    false
    //      (add-sub sub
    //        (unify t1 r2 -1 (list a2)))))
    //  (nil? a2)
    //  (if (== direction -1)
    //    (unify r1 t2 -1 (cons a1 sub))
    //      (if (const? a1)
    //        false
    //          (add-sub sub
    //            (unify r1 t2 1 (list a1)))))
    //  :else		; continue to main course, don't wanna ident
    //
    //  (case direction
    //  ;; Fresh position: neither side is consuming
    //  0	(cond
    //    (and (const? a1) (const? a2) (test= a1 a2))
    //    (unify r1 r2 0 [])
    //  (and (const? a1) (const? a2) (test-not= a1 a2))
    //  false
    //  (and (variable? a1) (const? a2))
    //  (fork-subs
    //    (add-sub (list a1)
    //      (unify r1 t2 0 ()))
    //  ;; Begin substitution { a2... / a1 }
    //  (unify r1 r2 1 (list a2,a1)))
    //  (and (const? a1) (variable? a2))
    //  (fork-subs
    //    (add-sub [a2]
    //      (unify t1 r2 0 ()))
    //  ;; Begin substitution { a1... / a2 }
    //  (unify r1 r2 -1 (list a1,a2)))
    //  :else
    //  ;; Either a1 consumes a2 or vice versa
    //  (fork-subs (unify r1 r2  1 (list a2,a1))
    //  (unify r1 r2 -1 (list a1,a2))))
    //  ;; A variable (X) on the left is consuming
    //    1	(cond
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
    new Subs
  }

//
//  ;; Is x a constant?
//  ;; At this point x must be Atom (defined in Java)
//  ;; Return Yes if x.index >= 0
//  (defn const? [x]
//    (>= (.index x) 0))
//
//  ;; Is x a variable?
//  ;; At this point x must be Atom (defined in Java)
//  ;; Return Yes if x.index < 0
//  (defn variable? [x]
//    (< (.index x) 0))
//
//  ;; Test equality of 2 Atoms
//  ;; Both Atoms are known to be constants
//  (defn test= [x y]
//  (cond (and (= (.index x) 0) (= (.index y) 0))
//  (= (.str x) (.str y))
//  :else
//  (= (.index x) (.index y))))
//
//  ;; Test inequality of 2 Atoms
//  ;; Both Atoms are known to be constants
//  (defn test-not= [x y]
//  (cond (and (= (.index x) 0) (= (.index y) 0))
//  (not= (.str x) (.str y))
//  :else
//  (not= (.index x) (.index y))))
//
//  ;; Merge 2 lists of compound substitutions
//  ;; -- semantics is OR
//  ;; -- INPUT: each of x, y is a list of compound subs
//  ;; -- a compound sub is a set
//  ;; -- OUTPUT: a list of compound subs
//  (defn fork-subs [x y]
//    (cond
//      (false? x)
//      y
//      (false? y)
//      x
//      :else
//  (concat x y)))
//
//  ;; Add an atomic substitution to a list of compound substitutions
//  ;; -- semantics is AND, distributed into the list
//  ;; -- INPUT: x is an atomic sub, as a list (not set)
//  ;;           y is a list of compound subs, ie, a list of sets
//  ;;           On input, x needs to be reversed because it was built up via cons
//  ;; -- OUTPUT: a list of compound subs
//  (defn add-sub [x y]
//    (cond
//      (false? x)
//  false
//  (false? y)
//  false
//  (empty? x)
//  y
//  (empty? y)
//  (list #{(reverse x)})
//  :else
//  (for [y1 y]		; for each compound sub in y
//  (if (empty? (first y1))
//  #{(reverse x)}
//  ;; do the set union
//  (set/union #{(reverse x)} y1)))))

}
