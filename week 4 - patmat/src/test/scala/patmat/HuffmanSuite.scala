package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val t3 = Leaf('e', 1)
    val t4 = Leaf('t', 2)
    val t5 = Leaf('x', 4)
    val t6 = Fork(Leaf('e', 4), Fork(Leaf('f', 2), Leaf('g', 3), List('f', 'g'), 5), List('e', 'f', 'g'), 9)
    val leaflist = List(t3, t4, t5)
    val t2Table: CodeTable = List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1)))
    val t6Table: CodeTable = List(('e', List(0)), ('f', List(1, 0)), ('g', List(1, 1)))
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("make code tree") {
    new TestTrees {
      val result = makeCodeTree(t1, t2)
      assert(result === new Fork(t1, t2, List('a', 'b', 'a', 'b', 'd'), 14))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(List('a', 'b', 'a'))") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)) || times(List('a', 'b', 'a')) === List(('b', 1), ('a', 2)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton true for a single code tree") {
    new TestTrees {
      assert(singleton(List(t1)) === true)
    }
  }
  test("singleton false for multiple code trees") {
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
    }
  }

  test("combine for some leaf list") {
    new TestTrees {
      assert(combine(leaflist) === List(Fork(t3, t4, List('e', 't'), 3), t5))
    }
  }

  test("until(singleton, combine)(trees)") {
    new TestTrees {
      assert(until(singleton, combine)(List(t1, t2)) === combine(List(t1, t2)))
      assert(until(singleton, combine)(leaflist) === List(Fork(Fork(t3, t4, List('e', 't'), 3), t5, List('e', 't', 'x'), 7)))
    }
  }

  test("createCodeTree(chars)") {
    new TestTrees {
      val a = Leaf('a', 2)
      val b = Leaf('b', 1)
      val c = Leaf('c', 1)
      assert(createCodeTree(List('a', 'a', 'b', 'c')) === Fork(Fork(c, b, List('c', 'b'), 2), a, List('c', 'b', 'a'), 4))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode a tree leo") {
    new TestTrees {
      assert(decode(t2, List(1, 0, 0, 0, 1)) === List('d', 'a', 'b'))

    }
  }

  test("encode a tree leo") {
    new TestTrees {
      assert(encode(t2)(List('d', 'a', 'b')) === List(1, 0, 0, 0, 1))
    }
  }

  test("codeBits leo") {
    new TestTrees {
      assert(codeBits(t2Table)('a') === List(0, 0))
      assert(codeBits(t2Table)('b') === List(0, 1))
      assert(codeBits(t2Table)('d') === List(1))
    }
  }

  test("convert leo") {
    new TestTrees {
      assert(convert(t2) === t2Table)
    }
  }

  test("mergeCodeTables leo") {
    new TestTrees {
      assert(mergeCodeTables(t2Table, t6Table) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1)), ('e', List(0)), ('f', List(1, 0)), ('g', List(1, 1))))
    }
  }
  test("quickEncode leo") {
    new TestTrees {
      assert(quickEncode(t2)(List('d', 'a', 'b')) === List(1, 0, 0, 0, 1))
    }
  }


}
