package fpis

import fpinscala.datastructures._

object Chapter3 extends App {

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(head, tail) => tail
    case Nil => Nil
  }

  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Cons(_, tail) => Cons(head, tail)
    case Nil => Nil
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _) => Nil
    case (_, 0) => l
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
      case Nil => acc
      case Cons(head, tail) => reverse(tail, Cons(head, acc))
    }
    reverse(tail(reverse(l)))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldLeft(as, 0)((count, _) => count + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((a, b) => f(b, a))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b, a) => f(a, b))

  def append[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)((a, acc) => Cons(a, acc))

  def flatten[A](lists: List[List[A]]): List[A] =
    foldRight(lists, Nil: List[A])(append)

  def increment(ns: List[Int]): List[Int] =
    foldRight(ns, Nil: List[Int])((n, acc) => Cons(n + 1, acc))

  def toStr(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  @annotation.tailrec
  def addEach(as: List[Int], bs: List[Int], acc: List[Int] = Nil): List[Int] =
    (as, bs) match {
      case (Nil, Nil) => reverse(acc)
      case (Nil, _) => append(acc, bs)
      case (_, Nil) => append(acc, as)
      case (Cons(a, as), Cons(b, bs)) => addEach(as, bs, Cons(a + b, acc))
    }

  @annotation.tailrec
  def zipWith[A, B, C](as: List[A], bs: List[B], acc: List[C] = Nil)(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _) => reverse(acc)
      case (_, Nil) => reverse(acc)
      case (Cons(a, as), Cons(b, bs)) => zipWith(as, bs, Cons(f(a, b), acc))(f)
    }
}
