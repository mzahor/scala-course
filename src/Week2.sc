def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

def sumQubes = sum(x => x * x)(_, _)

sumQubes(1, 4)

sum((a: Int) => a * a)(1, 4)

def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
}

product(x => x)(1, 4)

def print(data: Int) = {
  println("Executing")
  10
}

// pass by name will evaluate each time the name is used
def test(a: => Int) = {
  println(a)
  println(a)
}
test(print(10))



class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  }

  def neg() = {
    new Rational(-numer, denom)
  }

  def sub(that: Rational) = {
    this.add(that.neg())
  }

  override def toString = {
    s"${numer}/${denom}"
  }
}

new Rational(1, 3).sub(new Rational(5, 7)).sub(new Rational(3, 2))


