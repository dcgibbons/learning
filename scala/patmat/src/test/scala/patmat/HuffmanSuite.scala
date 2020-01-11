package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("makeCodeTree for simple tree") {
    val sampleTree = makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2)
    )
    assert(sampleTree == Fork(Fork(Leaf('x',1),Leaf('e',1),List('x', 'e'),2),Leaf('t',2),List('x', 'e', 't'),4))
  }

  test("createCodeTree for someText") {
    val sampleTree1 = createCodeTree("so".toList)
    assert(sampleTree1 == Fork(Leaf('o',1), Leaf('s', 1), List('o', 's'), 2))

    val sampleTree2 = createCodeTree("som".toList)
    assert(sampleTree2 == Fork(Leaf('m',1),Fork(Leaf('o',1),Leaf('s',1),List('o', 's'),2),List('m', 'o', 's'),3))

    val sampleTree3 = createCodeTree("some".toList)
    assert(sampleTree3 == Fork(Fork(Leaf('o',1),Leaf('s',1),List('o', 's'),2),Fork(Leaf('e',1),Leaf('m',1),List('e', 'm'),2),List('o', 's', 'e', 'm'),4))

    val sampleTree = createCodeTree("sometext".toList)
    val expectedTree = Fork(
      Fork(
        Fork(
          Leaf('o',1),
          Leaf('s',1),
          List('o', 's'),
          2),
        Fork(
          Leaf('x',1),
          Leaf('m',1),
          List('x', 'm'),
          2),
        List('o', 's', 'x', 'm'),
        4),
      Fork(
        Leaf('t',2),
        Leaf('e',2),
        List('t', 'e'),
        4),
      List('o', 's', 'x', 'm', 't', 'e'),
      8)
    assert(sampleTree == expectedTree)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decodedSecret") {
    val l = decodedSecret
    println("decodedSecret: " + l)
  }

  test("decode and quickEncode should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
