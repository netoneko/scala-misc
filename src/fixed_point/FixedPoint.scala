package fixed_point

import Math.abs

object FixedPoint extends App {
  val tolerance = 0.001

  def isCloseEnough(x: Double, y: Double) = abs((x - y) / (x * x)) < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double):Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  assert(fixedPoint(x => 1 + x / 2)(1) == 1.99609375)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double):Double = fixedPoint(averageDamp(y => x / y))(1)

  assert(sqrt(2) == 1.4142135623746899)
}
