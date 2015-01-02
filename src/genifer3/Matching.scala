package genifer3

// Matching is similar to unification, but without [dependent] variables.

// For example, in Prolog, different appearances of a variable X has to
// instantiate to the same object.  We don't name specific variables and
// thus do not have this kind of dependence.

// In Genifer3 there are no variables, but concepts can be generalized
// or specialized.  For example, "human" can generalize to "animal" or
// specialize to "john".  When doing matching, we need to look up a
// hierarchical tree of super-classes and sub-classes (ie, an ontology).

// One question is can we know beforehand that t1 is more general?
// The purpose of matching is to apply a rule:  one of the terms should
// be a pre-condition.  It cannot be that both terms are pre-conds.
// So we can assume one of the terms is intended to be more general.
// Let it be t1.

// Also, because of the lack of variable linkage (dependence), it seems
// that matching results are of no consequence -- we just need to know
// whether matching succeeds or not, and if yes, apply the post-condition
// of the rule.

// But now we have the problem of matching modulo rewriting, eg:
//    single ∙ young ∙ man = bachelor
// This seems to require term-rewriting techniques, similar to "narrowing".

// Perhaps we first simply implement matching of 2 terms without
// rewriting.

// ****** Below are YKY's personal notes ********
// Right now only 1 Atom can subsume another Atom
// In general, compound terms may subsume compound terms.
// For one Atom, we can get its parents, and then...?

// 首先，>* 係交由 function 處理... right?
// t1 = X a, X > 0     t2 = a
// t1 = X, X > a       t2 = a
// t2 = head . tail
// t1 = σ S = head . tail
// Maybe we need to detect if t1 contains a superclass term first?
// Or we find all subterms of t1, determine if they are superclass terms?
// What are examples of compound superclass? eg: my.friend, door.knob
// 但其實以前已經說過這些情況是需要 abduction 的。
// 那就不如暫時放棄 compound classes？
// 我們只處理 subsumption of atoms.

// 问题似乎是不能完全消灭 compound classes, 例如： 女教师.
// Inverses may be useful, to denote equivalence of 2 [possibly compound] terms.
// But then they merely denote short cuts?
// Seems that syntactic equations are all we need.

object Matching {

  // INPUT: t1 is assumed to be more general then t2 (because t1 belongs to the rule-to-be-applied)
  // OUTPUT: Boolean, whether a substitution σ exists such that σ t1 = t2.
  def matching(t1: ∏, t2: ∏): Boolean = {

    // Main algorithm
    // The main idea is to "eat" (consume) the formulas from their heads,
    // like 2 Pacmen:
    //      ᗧ person loves person       ᗧ john loves mary
    // and compare the heads and decide what to do.
    // The rest is pretty straightforward.
    // OUTPUT: true if t1 subsumes t2
      val a1 = t1.headOption
      val a2 = t2.headOption
      val r1 = if (t1.isEmpty) null else t1.tail
      val r2 = if (t2.isEmpty) null else t2.tail

      //  If either side is exhausted:
      if (a1.isEmpty && a2.isEmpty)
        true                    // success, regardless of direction
      else if (a1.isEmpty || a2.isEmpty)
        false
      else {
        // continue to main course

        // at this point we know that neither sides is None
        val a1_ = a1.orNull
        val a2_ = a2.orNull
        if (testEq(a1_, a2_))
          matching(r1, r2)
        else if (a1_.isConcept && a2_.isConcept && Ontology.⊃(a1_, a2_))
          matching(r1, r2)
        else
          false
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

  }