package int_sets

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x)

  def contains(x: Int):Boolean = false

  override def toString() = "."
}

class NonEmpty(v: Int, leftSet: IntSet, rightSet: IntSet) extends IntSet {
  def this(v: Int) = this(v, Empty, Empty)

  val value = v
  var left = leftSet
  var right = rightSet

  def contains(x: Int):Boolean = {
    if (this.value == x) true
    else if (this.value > x) left.contains(x)
    else left.contains(x)
  }

  def incl(x: Int):IntSet = {
    if (this.contains(x)) this
    else {
      def setLeaf(leaf: IntSet, assign: (IntSet) => Unit) = {
        if (leaf == Empty) {
          val newLeaf = new NonEmpty(x)
          assign(newLeaf)
          newLeaf
        }
        else leaf.incl(x)
      }

      if (x > this.value) setLeaf(this.right, (leaf) => this.right = leaf)
      else setLeaf(this.left, (leaf) => this.left = leaf)
    }
  }

  override def toString() = s"{${left}$value${right}}"
}

object Test extends App {
  val set = new NonEmpty(2)
  assert(set contains 2)

  set.incl(4)
  set.incl(1)

  assert(set contains 2)
  assert(set contains 1)

  assert(!set.contains(99))

  Array(99, 77, 3, 12).foreach(x => set.incl(x))
  println(set)

  assert(set.incl(200).asInstanceOf[NonEmpty].value == 200)
}
