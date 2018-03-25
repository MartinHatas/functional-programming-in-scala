class Poly(val terms: Map[Int, Double]) {

  def +(other: Poly): Poly = new Poly(terms ++ (other.terms map adjust))

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coef) = term
    terms get exp match {
      case Some(c) => (exp, c + coef)
      case None => term
    }
  }

  override def toString = (for{(exp, coef) <- terms.toList.sorted.reverse} yield s"${coef}x^$exp").mkString(" + ")
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))

p1 + p2