trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
     def generate: S = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  private val r = new java.util.Random
  override def generate = r.nextInt
}

integers.generate

val booleans = new Generator[Boolean] {
  override def generate = integers.generate > 0
}

booleans.generate

val booleans2 = for (i <- integers) yield (i > 0)

booleans2.generate