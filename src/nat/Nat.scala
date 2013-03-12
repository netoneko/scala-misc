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

  def iterate(step: Nat, result: Nat, f: (Nat) => Nat): Nat = {
    if (step.isZero) result else iterate(step.successor, f(result), f)
  }

  def + (that: Nat) = iterate(that, this, (x) => x.successor)
  def - (that: Nat) = iterate(that, this, (x) => x.predecessor)
}

object Test extends App {
  assert(Zero + Zero == Zero)

  val one = Zero.successor
  assert(one + Zero == one)
  assert(one.predecessor == Zero)

//  val two = one + one
//  assert(two.predecessor == one)
}
