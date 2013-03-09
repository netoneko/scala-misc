package rational

case class Rational(x: Int, y: Int) {
  private val divisor = gcd(x, y)

  val numer = x / divisor
  val denom = y / divisor

  def +(x: Rational) = {
    new Rational(numer * x.denom + x.numer * denom, denom * x.denom)
  }

  def -(x: Rational) = {
    this + x.neg()
  }

  def *(x: Rational) = {
    new Rational(numer * x.numer, denom * x.denom)
  }

  def /(x: Rational) = {
    this * Rational(x.denom, x.numer)
  }

  def ==(x: Rational) = {
    numer * x.denom == x.numer * denom
  }

  def neg() = {
    new Rational(-numer, denom)
  }

  override def toString() = s"$numer/$denom"

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }
}

object Test extends App {
  assert(Rational(4, 6).toString() == "2/3")
  assert(Rational(2, 3) == Rational(1, 3) == false)

  assert(Rational(3, 4) + Rational(2, 3) == Rational(9 + 8, 12))
  assert(Rational(3, 4) - Rational(2, 3) == Rational(9 - 8, 12))
  assert(Rational(3, 4) * Rational(2, 3) == Rational(6, 12))
  assert(Rational(3, 4) / Rational(2, 3) == Rational(9, 8))
  assert(Rational(3, 4).neg() == Rational(-3, 4))

  assert(Rational(3, 4) == Rational(6, 8))
}