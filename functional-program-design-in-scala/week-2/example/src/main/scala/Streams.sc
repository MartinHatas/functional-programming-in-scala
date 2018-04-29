

val intStream = (1 to 10).toStream

def from(i: Int): Stream[Int] = i #:: from(i + 1)

//from(20).take(100)

def sieve(numbers: Stream[Int]): Stream[Int] =
  numbers.head #:: sieve(numbers.tail.filter( n => n % numbers.head != 0))


val primes = sieve(from(2)).take(100).toList
