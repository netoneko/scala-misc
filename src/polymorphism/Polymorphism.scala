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

  def nth(x: Int): T = if (x != 0) tail.nth(x - 1) else head
}

object Polymorphism extends App {
  val list = new ConstantList[Int](3, new ConstantList[Int](4, new ConstantList[Int](7, new Nil[Int])))

  assert(list.nth(0) == 3)
  assert(list.nth(1) == 4)
  assert(list.nth(2) == 7)

  try   { list.nth(23) }
  catch { case e: Exception => assert(e.isInstanceOf[IndexOutOfBoundsException]) }
}
