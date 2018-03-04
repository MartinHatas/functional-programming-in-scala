class Rational(x: Int, y: Int) {

  require(y != 0, "Denominator must not be zero.")

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  private lazy val gcd: Int = gcd(x, y)

  val numerator: Int = x
  val denominator: Int = y

  def add(r: Rational) =
    new Rational(numerator * r.denominator + r.numerator * denominator, denominator * r.denominator)

  def neg = new Rational(-numerator, denominator)

  def subtract(r: Rational) = add(r.neg)

  def less(r: Rational) = numerator * r.denominator < r.numerator * denominator

  def max(r: Rational) = if(this less r) r else this

  override def toString = s"${numerator / gcd}/${denominator / gcd}"
}

val oneHalf = new Rational(1,2)
val twoThirds = new Rational(2,3)

oneHalf add twoThirds

oneHalf.neg

oneHalf subtract twoThirds

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x subtract y subtract z

new Rational(50, 100)

oneHalf less twoThirds

new Rational(16)