import scala.annotation.tailrec

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sum(x => x)(0, 10)
sum(x => x * x)(0, 10)