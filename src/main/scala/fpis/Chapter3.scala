package fpis

import fpinscala.datastructures._

object Chapter3 extends App {

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(head, tail) => tail
    case Nil              => Nil
  }

  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Cons(_, tail) => Cons(head, tail)
    case Nil           => Nil
  }
}