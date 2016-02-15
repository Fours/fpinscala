package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x1 = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => List(h)
      case Cons(h1, t) => Cons(h, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(h, t) => drop(t, n-1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if !f(h) => l
      case Cons(h, t) if f(h) => dropWhile(t, f)
    }
  }

  def init[A](l: List[A]): List[A] = {
    def go(as: List[A], acc: List[A]): List[A] = {
      as match {
        case Nil => acc
        case Cons(h, Nil) => acc
        case Cons(h, t) => go(t, append(acc, List(h)))
      }
    }
    go(l, Nil)
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, i) =>
      i + 1
    )

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(a, as) => foldLeft(as, f(z, a))(f)
    }

  // 3.11
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def prod3(ns: List[Int]) = foldLeft(ns, 1)(_ * _)

  def length2[A](ns: List[A]): Int = foldLeft(ns, 0)((l,a) => l+1)

  // 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((l, a) => append(List(a),l))

  // 3.13
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((x, y) => f(y,x))

  // 3.14

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((x,y) => Cons(y,x))

  // 3.15

  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(append)

  def concat2[A](ls: List[List[A]]): List[A] =
    foldLeft(reverse(ls), Nil: List[A])((x,y) => append(y,x))

  // 3.16

  def add1(ns: List[Int]): List[Int] =
    ns match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, add1(xs))
    }

  // 3.17

  def dubToString(ns: List[Double]): List[String] =
    ns match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, dubToString(xs))
    }

  // 3.18

  def map[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

  // 3.19

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(x, xs) => filter(xs)(f)
    }

  // 3.20

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Nil => Nil
      case Cons(x, xs) => append(f(x), flatMap(xs)(f))
    }

  // 3.21

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as) {
      case a if f(a) => List(a)
      case _ => Nil
    }

  // 3.22

  def addNumbers(ns1: List[Int], ns2: List[Int]): List[Int] =
    (ns1, ns2) match {
      case (Nil, Nil) => Nil
      case (Nil, xs) => xs
      case (xs, Nil) => xs
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addNumbers(xs, ys))
    }

  // 3.23

  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] =
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Nil, xs) => xs
      case (xs, Nil) => xs
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
    }

  // 3.24

  def hasSubSequence[A](l: List[A], s: List[A]): Boolean = {

    def startsWith(l2: List[A], s2: List[A]): Boolean =
      (l2, s2) match {
        case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(x, Nil), Cons(y, Nil)) if x == y => true
        case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
        case (_, _) => false
      }

    def tryNext(l3: List[A], s3: List[A]): Boolean =
      l3 match {
        case Nil => false
        case Cons(x, xs) =>
          startsWith(l3, s3) match {
            case true => true
            case false => tryNext(xs, s3)
          }
      }

    tryNext(l, s)
  }

}
