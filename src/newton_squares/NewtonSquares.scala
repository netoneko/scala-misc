package newton_squares
import Math.abs

object NewtonSquares extends App {
  def squares(x: Double, guess: Double = 1):Double =
    if (abs(guess * guess - x) / x < 0.001) guess
    else squares(x, (x / guess + guess) / 2)

  Array(2, 0.001, 0.1e-10, 1.0e20, 1.0e50).foreach { i => println(squares(i)) }
}
