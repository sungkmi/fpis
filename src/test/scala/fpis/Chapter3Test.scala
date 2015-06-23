package fpis

import org.scalatest._

import fpinscala.datastructures._
import Chapter3._

class Chapter3Test extends FunSuite {

  test("Exercise 3.2") {
    assert(tail(List(1, 2, 3, 4)) === List(2, 3, 4))
  }

  test("Exercise 3.3") {
    assert(setHead(List(1, 2, 3, 4), 10) === List(10, 2, 3, 4))
    assert(setHead(Nil, 10) === Nil)
  }

  test("Exercise 3.4") {
    assert(drop(List(1, 2, 3, 4), 3) === List(4))
  }

  test("Exercise 3.5") {
    assert(dropWhile[Int](List(1, 2, 3, 4), (_ < 3)) === List(3, 4))
  }

  test("Exercise 3.6") {
    assert(init[Int](List(1, 2, 3, 4)) === List(1, 2, 3))
  }
}
