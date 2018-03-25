

def isPrime(i: Int): Boolean = (2 until i) forall{x => i % x != 0}

val n = 7

(1 until n) flatMap  {i => (1 until i) map {j => (i, j)} } filter {case (i, j) => isPrime(i + j)}


for {
  i <- 1 until n
  j <- 1 until i
  if (isPrime(i + j))
} yield (i, j)