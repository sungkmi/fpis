package fpis

import fpinscala.datastructures._

object Chapter3 extends App {
  
  def tail[A](as: List[A]): List[A] = as match {
    case Cons(head, tail) => tail
    case Nil => Nil    
  }
}