package fpis

import org.scalatest._

import fpinscala.datastructures._
import Chapter3._

class Chapter3Test extends FunSuite {

  test("Exercise 3.2") {
    assert(tail(List(1, 2, 3, 4)) === List(2,3,4))
  }
}