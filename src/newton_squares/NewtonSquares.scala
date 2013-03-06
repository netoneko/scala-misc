package newton_squares

object NewtonSquares extends App {
  def abs(x: Double) = if (x > 0) x else -x

  def isGoodEnough(guess: Double, x: Double) = {
    abs(guess * guess - x) < 0.001
  }

  def improve(guess: Double, x: Double) = {
    (x / guess + guess) / 2
  }

  def squares(guess: Double, x: Double):Double =
    if (isGoodEnough(guess, x)) guess
    else squares(improve(guess, x), x)

  val squareOfTwo = squares(1.0, 2.0)
  assert(squareOfTwo == 1.4142156862745097)
}
