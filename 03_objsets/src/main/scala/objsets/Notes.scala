package notes


object Session{

  abstract class IntSet {
    def include(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(set: IntSet): IntSet
    // `union was little bit tricky to introduce.  I was able to
    // narrow return type for include, sine we still return same tape,
    // but here for parameter it is not possible.  `union` needs to be
    // able to handle all `IntSets`, not just `IntTree`, and it was a
    // problem for me since I wanted to access `left` and `right` fields
    // onto the `other` parameter.  And that was my fallacy.
    //
    // I should not think about data and internal representation, even
    // if it's available.  All I should be thinking is interface,
    // behaviors that can be used to modify `other` object.  Don't
    // break the barrier of abstraction.
    //
    // I now seems obvious, but once you get going into the details of
    // implementation, it is easy to forget about what you are really
    // doing.
  }

  abstract class IntTree extends IntSet {
    override def include(x: Int): IntTree
  }

  object EmptyLeaf extends IntTree {
    def contains(x:Int) = false
    def include(x: Int) = new Leaf(x, EmptyLeaf, EmptyLeaf)
    def union(other: IntSet) = other

    override def toString() = "."
  }

  class Leaf(
    element: Int,
    left: IntTree,
    right: IntTree)
      extends IntTree {

    def contains(x: Int): Boolean =
      if (x == element) true
      else if (x < element) left contains x
      else  right contains x

    def include(x: Int): IntTree =
      if (x < element)
        new Leaf(element, left include x, right)
      else if (x > element)
        new Leaf(element, left, right include x)
      else
        this

    def union(other: IntSet): IntSet =
      left union right union other include element


    override def toString() =
      "(" + left + element.toString() + right + ")"
  }
}
