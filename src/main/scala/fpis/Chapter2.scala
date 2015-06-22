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
}