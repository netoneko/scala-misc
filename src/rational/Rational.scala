package rational

case class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private val divisor = gcd(x, y)

  val numer = x / divisor
  val denom = y / divisor


  def +(x: Rational) = new Rational(numer * x.denom + x.numer * denom, denom * x.denom)

  def -(x: Rational) = this + -x

  def *(x: Rational) = new Rational(numer * x.numer, denom * x.denom)

  def /(x: Rational) = this * Rational(x.denom, x.numer)

  def ==(x: Rational):Boolean = numer * x.denom == x.numer * denom

  def >(x: Rational):Boolean = (this - x).numer > 0

  def >=(x: Rational) = this > x || this == x

  def <(x: Rational):Boolean = (x - this).numer > 0

  def <=(x: Rational) = this < x || this == x

  def unary_-() = {
    new Rational(-numer, denom)
  }

  override def toString() = s"$numer/$denom"

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }
}

object Rational {
  def apply(x: Int) = new Rational(x)
}

object Test extends App {
  assert(Rational(4, 6).toString() == "2/3")
  assert(Rational(7).toString() == "7/1")

  assert(Rational(2, 3) == Rational(1, 3) == false)

  assert(Rational(3, 4) + Rational(2, 3) == Rational(9 + 8, 12))
  assert(Rational(3, 4) - Rational(2, 3) == Rational(9 - 8, 12))
  assert(Rational(3, 4) * Rational(2, 3) == Rational(6, 12))
  assert(Rational(3, 4) / Rational(2, 3) == Rational(9, 8))

  assert(-Rational(3, 4) == Rational(-3, 4))
  assert(Rational(3, 4) == Rational(6, 8))

  assert(Rational(3, 4) > Rational(1, 2))
  assert(Rational(3, 4) >= Rational(1, 2) && Rational(1, 2) >= Rational(1, 2))

  assert(Rational(3, 4) < Rational(7, 8))
  assert(Rational(3, 4) <= Rational(7, 8) && Rational(1, 2) <= Rational(1, 2))

  assert(Rational(-1, 2) > Rational(-2, 3))
}