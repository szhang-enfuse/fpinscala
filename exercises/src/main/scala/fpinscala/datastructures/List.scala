package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def main(args: Array[String]): Unit = {
    println(x)
    val l: List[Int] = List(1, 2, 3, 4, 5, -3, 6, 9)
    println(drop(l, 2))
    println(dropWhile(l, (x: Int) => x < 4))
    println(init(l))
    val d = List(1.0, 2.1, 3.0, 1.0, 4.2, 5.4)
    println(product3(d))
    println("---foldRight----")
    println(foldRight(l, Nil: List[Int])(Cons(_, _)))
    println("---foldLeft----")
    println(foldLeft(l, Nil: List[Int])((y, x) => Cons(x, y)))
    println("---reserve----")
    println(reverse(l))
    println(length(l))
    println(sum4(l))
    println(product4(d))
    println(appendwFoldRight(l, List(10, 11, 12)))
    println(appendwFoldLeft(l, List(10, 11, 12)))
    val cl = concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9, 10)))
    println(cl)
    println(map(cl)(_ + 1))

  }

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(ns: List[Double]) = ns match {
    case Cons(0.0, t) => {
      println("found 0.0")
      0.0
    }
    case _ => foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum4(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product4(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def reverse[A](ns: List[A]): List[A] = foldLeft(ns, Nil: List[A])((y, x) => Cons(x, y))

  def appendwFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def appendwFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((x, y) => Cons(y, x))

  def concat[A](a1: List[List[A]]): List[A] = foldRight2(a1, Nil: List[A])(appendwFoldLeft)

  def addOne(l: List[Int]): List[Int] = foldRight2(l, Nil: List[Int])((x, y) => Cons(x + 1, y))

  def toString(l: List[Double]): List[String] = foldRight2(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight2(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = ???
}
