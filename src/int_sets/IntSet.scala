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
    else if (x > this.value) right.contains(x)
    else left.contains(x)
  }

  // Regards to https://gist.github.com/uniquelogin/21180feca84ac11f8d84
  def incl(x: Int):IntSet = {
    if (x > this.value) { this.right = this.right.incl(x) }
    else if (x < this.value) { this.left = this.left.incl(x); }

    this;
  }

  override def toString() = s"{${left}$value${right}}"
}

object Test extends App {
  val set = new NonEmpty(2)
  assert(set contains 2)

  set.incl(4)
  set.incl(1)

  assert(set contains 4)
  assert(set contains 1)

  assert(!set.contains(99))

  Array(99, 77, 3, 12).foreach(x => set.incl(x))
  println(set)
}
