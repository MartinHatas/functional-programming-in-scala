trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Nil[T] extends List[T] {
  override def isEmpty = ???
  override def head = ???
  override def tail = ???
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = ???
}