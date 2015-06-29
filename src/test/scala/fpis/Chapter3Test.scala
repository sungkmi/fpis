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
    assert(init(List(1, 2, 3, 4)) === List(1, 2, 3))
  }

  ignore("Exercise 3.9") {
    assert(length[Int](List(1, 2, 3)) === 3)
  }

  test("Exercise 3.10") {
    assert(foldLeft(List(1, 2, 3), 0)(_ + _) === 6)
  }

  test("Exercise 3.11") {
    assert(sum(List(1, 2, 3)) === 6)
    assert(product(List(1, 2, 3)) === 6)
    assert(length(List(1, 2, 3)) === 3)
  }

  test("Exercise 3.12") {
    assert(reverse(List(1, 2, 3)) === List(3, 2, 1))
  }

  test("Exercise 3.14") {
    assert(append(List(1, 2, 3), List(4, 5, 6)) === List(1, 2, 3, 4, 5, 6))
  }

  test("Exercise 3.15") {
    assert(flatten(List(List(1, 2), List(3, 4))) === List(1, 2, 3, 4))
  }

  test("Exercise 3.16") {
    assert(increment(List(1, 2, 3)) === List(2, 3, 4))
  }

  test("Exercise 3.17") {
    assert(toStr(List(1.0, 2.0, 3.0)) === List("1.0", "2.0", "3.0"))
  }

  test("Exercise 3.18") {
    assert(map(List(1, 2, 3))(_ + 1) === List(2, 3, 4))
  }

  test("Exercise 3.19") {
    assert(filter(List(1, 2, 3, 4))(_ % 2 == 0) === List(2, 4))
  }

  test("Exercise 3.20") {
    assert(flatMap(List(1, 2, 3))(i => List(i, i)) === List(1, 1, 2, 2, 3, 3))
  }
  
  test("Exercise 3.21") {
    assert(filter2(List(1, 2, 3, 4))(_ % 2 == 0) === List(2, 4))
  }

}
