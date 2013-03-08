package factorial

object Factorial extends App {
  def factorial(i: Integer): Integer =
    if (i == 0) 1 else i * factorial(i - 1)

  assert(factorial(4) == 24)
}
