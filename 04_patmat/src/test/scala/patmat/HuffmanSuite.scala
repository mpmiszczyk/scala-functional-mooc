package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(
      Leaf('a',2),                                 // 0
      Leaf('b',3), List('a','b'), 5)               // 1
    val t2 =
      Fork(
        Fork(
          Leaf('a',2),                            // 0,0
          Leaf('b',3), List('a','b'), 5),         // 0,1
        Leaf('d',4), List('a','b','d'), 9)        // 1
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

  test("times will return list of pairs of char and count") {
    assert(times(List('a','b','a')) === List(('a',2), ('b', 1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) ===
      List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("conver CodeTree to CodeTable") {
    new TestTrees{
      val ct_t1: CodeTable = convert(t1)

      assert(ct_t1.size === 2)
      assert(ct_t1 contains (('a',List(0))))
      assert(ct_t1 contains (('b',List(1))))

      val ct_t2: CodeTable =  convert(t2)
      assert(ct_t2.size === 3)
      assert(ct_t2 contains (('a', List(0,0))))
      assert(ct_t2 contains (('d', List(1))))
    }
  }

  test("encode string") {
    new TestTrees {
      assert(encode(t1)("ab".toList) == List(0,1))
      assert(encode(t1)("abb".toList) == List(0,1,1))

      assert(encode(t2)("a".toList) === List(0,0))
      assert(encode(t2)("b".toList) === List(0,1))
      assert(encode(t2)("d".toList) === List(1))
      assert(encode(t2)("abbd".toList) == List(0,0) ::: List(0,1) ::: List(0,1) ::: List(1))
    }
  }

  test("decode bit list to string") {
    new TestTrees {
      assert(decode(t1, List(0)) === "a".toList)
      assert(decode(t1, List(1)) === "b".toList)


    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
