trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def get(i: Int): T

}

object Nil extends List[Nothing] {
  override def isEmpty = true
  override def head: Nothing = throw new NoSuchElementException
  override def tail: Nothing = throw new NoSuchElementException
  override def get(i: Int): Nothing = throw new IndexOutOfBoundsException
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false

  override def get(i: Int) =
    if (i == 0) head
    else tail.get(i - 1)
}


val test: List[String] = Nil