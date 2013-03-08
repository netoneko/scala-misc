package curry

object Curry extends App {
  def sum(f: Int => Int)(a: Int, b: Int):Int = {
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)
  }

  assert(sum(x => x)(2, 5) == 14)

  def product(f: Int => Int)(a: Int, b: Int):Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  assert(product(x => x * x)(3, 7) == 6350400)

  def factorial(n: Int) = product(x => x)(1, n)

  assert(factorial(4) == 24)
}
