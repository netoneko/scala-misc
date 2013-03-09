package rational

case class Rational(x: Int, y: Int) {
  val numer = x
  val denom = y

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

  override def toString() = {
    val divider = gcd(numer, denom)
    (numer / divider) + "/" + (denom / divider)
  }

  private def gcd(a: Int, b: Int): Int = {
    if (a == b) a
    else if (a > b) gcd(a - b, a)
    else gcd(a, b - a)
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