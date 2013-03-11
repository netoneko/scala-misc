package polymorphism

import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(x: Int): T
}

class Nil[T] extends List[T] {
  def isEmpty = true

  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")

  def nth(x: Int): T = throw new IndexOutOfBoundsException
}

class ConstantList[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

  def nth(x: Int): T = {
    def iterate(list: List[T], n: Int): T = {
      if (n != x) iterate(list.tail, n - 1) else list.head
    }

    iterate(this, 0)
  }
}

object Polymorphism extends App {
  val list = new ConstantList[Int](3, new ConstantList[Int](4, new ConstantList[Int](7, new Nil[Int])))

  assert(list.nth(0) == 3)
}
