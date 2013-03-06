package newton_squares
import Math.abs

object NewtonSquares extends App {
  def squares(guess: Double, x: Double):Double =
    if (abs(guess * guess - x) < 0.001) guess
    else squares((x / guess + guess) / 2, x)

  val squareOfTwo = squares(1.0, 2.0)
  println(squareOfTwo)
  assert(squareOfTwo == 1.4142156862745097)
}
