package rational

case class Rational(x: Int, y: Int) {
  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  def +(x: Rational) = {
    new Rational(numer * x.denom + x.numer * denom, denom * x.denom)
  }

  def -(x: Rational) = {
    new Rational(numer * x.denom - x.numer * denom, denom * x.denom)
  }

  def *(x: Rational) = {
    new Rational(numer * x.numer, denom * x.denom)
  }

  def /(x: Rational) = {
    new Rational(numer * x.denom, denom * x.numer)
  }

  def ==(x: Rational) = {
    numer * x.denom == x.numer * denom
  }

  override def toString() = s"$numer/$denom"

  private def gcd(a: Int, b: Int): Int = {
    if (a == b) a
    else if (a > b) gcd(a - b, a)
    else gcd(a, b - a)
  }
}

object Test extends App {
  assert(Rational(2, 3).toString() == "2/3")
  assert(Rational(2, 3) == Rational(1, 3) == false)

  assert(Rational(3, 4) + Rational(2, 3) == Rational(9 + 8, 12))
  assert(Rational(3, 4) - Rational(2, 3) == Rational(9 - 8, 12))
  assert(Rational(3, 4) * Rational(2, 3) == Rational(6, 12))
  assert(Rational(3, 4) / Rational(2, 3) == Rational(9, 8))

  assert(Rational(3, 4) == Rational(6, 8))
}