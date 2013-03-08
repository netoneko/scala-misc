package sum

object Sum extends App {
  def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int):Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }

    loop(a, 0)
  }

  assert(sum(x => x * x, 3, 5) == 50)
}
