package int_sets

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

class Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x)

  def contains(x: Int):Boolean = false

  override def toString() = ""
}

class NonEmpty(v: Int, leftSet: IntSet, rightSet: IntSet) extends IntSet {
  def this(v: Int) = this(v, new Empty, new Empty)

  val value = v
  var left = leftSet
  var right = rightSet

  def contains(x: Int):Boolean = if (this.value == x) true else left.contains(x) || right.contains(x)

  def incl(x: Int):IntSet = {
    if (this.contains(x)) this
    else {
      if (x > this.value) {
        if (right.isInstanceOf[Empty]) { right = new NonEmpty(x); right }
        else right.incl(x)
      } else {
        if (left.isInstanceOf[Empty]) { left = new NonEmpty(x); left }
        else left.incl(x)
      }
    }
  }

  override def toString() = {
    s"$value[${left}][${right}]"
  }
}

object Test extends App {
  val set = new NonEmpty(2)
  assert(set contains 2)

  set.incl(4)
  set.incl(1)

  assert(set contains 2)
  assert(set contains 1)

  assert(!set.contains(99))

  set.incl(99)
  set.incl(77)
  set.incl(12)
  set.incl(3)


  println(set)
}
