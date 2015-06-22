package fpis

import org.scalatest._

import Chapter2._

class Chapter2Test extends FunSuite {

  test("Exercise 2.1") {
    assert(fib(1) === 0)
    assert(fib(2) === 1)
    assert(fib(3) === 1)
    assert((1 to 6 map fib) === Seq(0, 1, 1, 2, 3, 5))
  }
}