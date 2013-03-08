package curry

object Curry extends App {
  def mapReduce(value: Int, combine: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int):Int = {
    if (a > b) value
    else combine(f(a), mapReduce(value, combine)(f)(a + 1, b))
  }

  def sum(a: Int, b: Int):Int = mapReduce(0, (x, y) => x + y)(x => x)(a, b)
  assert(sum(2, 5) == 14)

  def product(f: Int => Int)(a: Int, b: Int):Int = mapReduce(1, (x, y) => x * y)(f)(a, b)
  assert(product(x => x * x)(3, 7) == 6350400)

  def factorial(n: Int) = product(x => x)(1, n)
  assert(factorial(4) == 24)
}
