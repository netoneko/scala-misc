package newton_squares
import Math.abs

object NewtonSquares extends App {
  def squares(x: Double, guess: Double = 1):Double =
    if (abs(guess * guess - x) < 0.001) guess
    else squares(x, (x / guess + guess) / 2)

  val squareOfTwo = squares(2)
  println(squareOfTwo)
  assert(squareOfTwo == 1.4142156862745097)
}
