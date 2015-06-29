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

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _)              => Nil
    case (_, 0)                => l
    case (Cons(head, tail), _) => drop(tail, n - 1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) if f(head) =>
      dropWhile(tail, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def reverse(l: List[A], acc: List[A] = Nil): List[A] = l match {
      case Nil              => acc
      case Cons(head, tail) => reverse(tail, Cons(head, acc))
    }
    reverse(tail(reverse(l)))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, count) => count + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = 
    as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
}
