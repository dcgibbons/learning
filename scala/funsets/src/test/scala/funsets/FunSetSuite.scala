package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains all elements that are in both sets") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val u = intersect(s, t)
      assert(contains(u, 2), "Intersect 1")
      assert(!contains(u, 1), "Intersect 2")
      assert(!contains(u, 3), "Intersect 3")
    }
  }

  test("diff determines the element that are in one set but not the other") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val v = diff(s, t)
      assert(contains(v, 1), "Diff 1")
      assert(!contains(v, 3), "Diff 2")
      assert(!contains(v, 2), "Diff 3")
    }
  }

  test("filter filters elements in a set") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val u = intersect(s, t)
      val v = filter(u, (e: Int) => e % 2 == 0)
      assert(contains(v, 2), "Filter 1")
      assert(!contains(v, 1), "Filter 2")
      assert(!contains(v, 3), "Filter 3")
    }
  }

  test("forall bounded integer test") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val u = intersect(s, t)
      val v = singletonSet(99)
      assert(forall(u, (e:Int) => e < 100), "forall 1")
      assert(!forall(v, (e:Int) => e < 10), "forall 2")

      val a = singletonSet(1)
      val b = singletonSet(3)
      val c = singletonSet(4)
      val d = singletonSet(5)
      val e = singletonSet(7)
      val f = singletonSet(1000)
      val g = union(a, b)
      val h = union(g, c)
      val i = union(h, d)
      val j = union(i, e)
      val k = union(j, f)
      assert(!forall(k, (e: Int) => e < 5), "all integers less than 5")
    }
  }

  test("exists bounded integer test") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val u = intersect(s, t)
      assert(exists(u, (e: Int) => e % 2 == 0), "exists 1")
      assert(!exists(u, (e: Int) => e % 2 != 0), "exists 2")
      assert(!exists(u, (e: Int) => e > 10), "exists 3")
    }
  }

  test("map test") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val u = intersect(s, t)
      val v = map(u, (x:Int) => x + 100)
      //assert(contains(v, 101), "contains 1")
      //assert(contains(v, 102), "contains 2")
      //assert(contains(v, 103), "contains 3")
      //assert(!contains(v, 1), "contains 4")
    }
  }

}
