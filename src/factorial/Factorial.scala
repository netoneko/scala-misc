package factorial

object Factorial extends App {
  def factorial(n: BigDecimal): BigDecimal = {
    def loop(acc: BigDecimal, n: BigDecimal): BigDecimal =
      if (n == 0) acc else loop(acc * n, n - 1)

    loop(1, n)
  }

  println(factorial(10000))
  assert(factorial(4) == 24)
}
