import scala.annotation.tailrec

// implementation of tail-recursive version of factorial function
def factorial(a: Int): Int = {
  @tailrec
  def fact(acc: Int, max: Int, curr: Int): Int = {
    if (curr == a) acc * curr
    else
      fact(acc * curr, a, curr + 1)
  }

  fact(1, a, 1)
}
factorial(6)


def pascal(c: Int, r: Int): Int = {
  if (c == 0 || r == c) 1
  else pascal(c - 1, r - 1) + pascal(c, r - 1)
}

def balance(chars: List[Char]): Boolean = {
  val filtered = chars.filter(x => x.equals('(') || x.equals(')'))

  def isBalanced(acc: Int, seq: List[Char]): Boolean = {
    if (acc < 0) false
    else if (seq.isEmpty) {
      if (acc == 0) true else false
    }
    else isBalanced(if (seq.head.equals('(')) acc + 1 else acc - 1, seq.tail)
  }

  isBalanced(0, filtered)
}

def countChange(money: Int, coins: List[Int]): Int = {
  if(money == 0) 1
  else if (money < 0) 0
  else if (coins.isEmpty) 0
  else {
    countChange(money - coins.head, coins) +
      countChange(money, coins.tail)
  }
}





