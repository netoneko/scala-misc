package curry

object Curry extends App {
  def product(f: Int => Int)(a: Int, b: Int):Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  assert(product(x => x * x)(3, 7) == 6350400)
}
