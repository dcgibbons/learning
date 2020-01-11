package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = {
      (e: Int) => e == elem
    }

    /**
      * Returns the union of the two given sets,
      * the sets of all elements that are in either `s` or `t`.
      */
    def union(s: Set, t: Set): Set = {
      (e: Int) => contains(s, e) || contains(t, e)
    }

    /**
      * Returns the intersection of the two given sets,
      * the set of all elements that are both in `s` and `t`.
      */
    def intersect(s: Set, t: Set): Set = {
      (e: Int) => contains(s, e) && contains(t, e)
    }


    /**
      * Returns the difference of the two given sets,
      * the set of all elements of `s` that are not in `t`.
      */
    def diff(s: Set, t: Set): Set = {
      (e: Int) => contains(s, e) && !contains(t, e)
    }

    /**
      * Returns the subset of `s` for which `p` holds.
      */
    def filter(s: Set, p: Int => Boolean): Set = {
      (e: Int) => contains(s, e) && p(e)
    }


    /**
      * The bounds for `forall` and `exists` are +/- 1000.
      */
    val bound = 1000

    /**
      * Returns whether all bounded integers within `s` satisfy `p`.
      */
    def forall(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if (a > bound) true
        else if (contains(s, a) && !p(a)) false
        else iter(a + 1)
      }
      iter(-bound)
    }

    /**
      * Returns whether there exists a bounded integer within `s`
      * that satisfies `p`.
      */
    def exists(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if (a > bound) false
        else if (contains(s, a) && forall(singletonSet(a), p)) true
        else iter(a + 1)
      }
      iter(-bound)
    }

    /**
      * Returns a set transformed by applying `f` to each element of `s`.
      */
    def map(s: Set, f: Int => Int): Set = {
      // serious crap
      def iter(s2: Set, a: Int): Set = {
        if (a > bound) {
          s2
        } else {
          val s3 = if (contains(s, a)) union(s2, singletonSet(f(a))) else s2
          iter(s3, a + 1)
        }
      }
      iter(singletonSet(bound + 1), -bound)

      /*
      var newSet = singletonSet(bound + 1)
      for (i <- -bound to bound) {
        if (contains(s, i)) {
          val s2 = singletonSet(f(i))
          newSet = union(s2, newSet)
        }
      }
      newSet
      */
    }

    /**
      * Displays the contents of a set
      */
    def toString(s: Set): String = {
      val xs = for (i <- -bound to bound if contains(s, i)) yield i
      xs.mkString("{", ",", "}")
    }

    /**
      * Prints the contents of a set on the console.
      */
    def printSet(s: Set) {
      println(toString(s))
    }
}
