abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
}

object EmptySet extends IntSet{
  override def contains(x: Int) = false
  override def incl(x: Int) = new NonEmptySet(x, EmptySet, EmptySet)

  override def toString = "."

  override def union(other: IntSet) = other
}

class NonEmptySet(e: Int, left: IntSet, right: IntSet) extends IntSet {

  override def contains(x: Int) = x match {
    case x if x < e => left.contains(x)
    case x if x > e => right.contains(x)
    case _ => true
  }

  override def incl(x: Int) = x match {
    case x if x < e => new NonEmptySet(e, left incl x, right)
    case x if x > e => new NonEmptySet(e, left, right incl x)
    case _ => this
  }

  override def toString = s"{$left$e$right}"

  override def union(other: IntSet) = ((left union right) union other ) incl e
}

val s5 = new NonEmptySet(5, EmptySet, EmptySet)

s5.incl(3).incl(7).incl(6)

val s2 = new NonEmptySet(7, EmptySet, EmptySet) incl(2)

s5.union(s2)