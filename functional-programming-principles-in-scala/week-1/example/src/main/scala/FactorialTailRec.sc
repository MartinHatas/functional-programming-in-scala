import scala.annotation.tailrec

def factorial(n: Int): Int = {
  @tailrec
  def factorialAccumulator(n: Int, acc: Int): Int = {
    if (n == 0) acc else factorialAccumulator(n - 1, acc * n)
  }
  factorialAccumulator(n, 1)
}


factorial(5) //120
factorial(6) //720