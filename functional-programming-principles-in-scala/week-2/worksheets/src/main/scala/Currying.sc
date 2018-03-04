def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
}

val simpleProduct = product(x => x) (3, 7)
val squareProduct = product(x => x * x) (3, 7)


def fact(n: Int) = product(x => x) (1, n)

fact(5)


def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sum(f)(a + 1, b)
}


def general(f: Int => Int, unitValue: Int, operation: (Int, Int) => Int) (a: Int, b: Int): Int = {
  if (a > b) unitValue
  else operation(f(a), general(f, unitValue, operation)(a + 1, b))
}

def product2(f: Int => Int)(a: Int, b: Int): Int = general(f, 1, (x,y) => x*y) (a, b)
val simpleProduct2 = product(x => x) (3, 7)

def sum2(f: Int => Int)(a: Int, b: Int): Int = general(f, 0, (x,y) => x + y)(a,b)
sum2(x => 1)(0,3)