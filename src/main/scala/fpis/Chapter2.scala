package fpis

object Chapter2 extends App {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibTuple(i: Int, last: (Int, Int)): (Int, Int) =
      if (i == n) last
      else {
        fibTuple(i + 1, (last._2, last._1 + last._2))
      }

    fibTuple(1, (0, 1))._1
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n == as.size - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    if (as.size < 2) true else loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A =>
      b: B => f(a, b)
  }
}