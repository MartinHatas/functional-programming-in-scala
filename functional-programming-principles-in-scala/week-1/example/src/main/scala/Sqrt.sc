def sqrt(x: Double) = {

  def abs(x:Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) = abs(guess * guess - x) < x / 1000

  def improve(guess: Double) = (guess + x / guess) / 2

  sqrtIter(1.0)
}


val a = sqrt(0.001)
val b = sqrt(0.1e-20)
val c = sqrt(1.0e20)
val d = sqrt(1.0e50)