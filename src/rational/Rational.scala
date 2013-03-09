package rational

case class Rational(x: Int, y: Int) {
  val numer = x
  val denom = y

  def +(x: Rational) = {
    new Rational(numer * x.denom + x.numer * denom, denom * x.denom)
  }

  def -(x: Rational) = {
    new Rational(numer * x.denom - x.numer * denom, denom * x.denom)
  }

  def ==(x: Rational) = {
    numer == x.numer && denom == x.denom
  }

  override def toString() = s"$numer/$denom"
}

object Test extends App {
  assert(Rational(2, 3).toString() == "2/3")
  assert(Rational(2, 3) == Rational(1, 3) == false)

  assert(Rational(3, 4) + Rational(2, 3) == Rational(9 + 8, 12))
  assert(Rational(3, 4) - Rational(2, 3) == Rational(9 - 8, 12))

}