package nat

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true

  def predecessor = throw new NoSuchElementException("Zero has no predecessors!")
  def successor = new Successor(Zero)

  def + (that: Nat) = that
  def - (that: Nat) = that
}

class Successor(n: Nat) extends Nat {
  def isZero = false

  def predecessor = n
  def successor = new Successor(this)

  def + (that: Nat) = new Successor(n + that)
  def - (that: Nat) = new Successor(n - that)
}

class Test extends App {

}
