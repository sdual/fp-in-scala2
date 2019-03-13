package com.github.sdual.fpinscala.chap03

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    @tailrec
    def loop(sup1: List[A], sub1: List[A]): Boolean = {
      sup1 match {
        case Nil => sub1 match {
          case Nil => true
          case _   => false
        }
        case Cons(x1, xs1) => sub1 match {
          case Nil => true
          case Cons(x2, xs2) if !(x1 == x2) => loop(xs1, sub1)
          case Cons(x2, xs2) => loop(xs1, xs2)
        }
      }
    }

    loop(sup, sub)
  }

  // Exercise 3.23
  def zip2[A](as1: List[A], as2: List[A], op: (A, A) => A): List[A] = {
    @tailrec
    def loop(bs1: List[A], bs2: List[A], acc: List[A]): List[A] = {
      bs1 match {
        case Nil => acc
        case Cons(x1, xs1) => bs2 match {
          case Nil => acc
          case Cons(x2, xs2) => loop(xs1, xs2, Cons(op(x1, x2), acc))
        }
      }
    }

    List.reverse(loop(as1, as2, Nil))
  }

  // Exercise 3.22
  def zip(as1: List[Int], as2: List[Int]): List[Int] = {
    @tailrec
    def loop(bs1: List[Int], bs2: List[Int], acc: List[Int]): List[Int] = {
      bs1 match {
        case Nil => acc
        case Cons(x1, xs1) => bs2 match {
          case Nil => acc
          case Cons(x2, xs2) => loop(xs1, xs2, Cons(x1 + x2, acc))
        }
      }
    }

    List.reverse(loop(as1, as2, Nil))
  }

  // Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x =>
      if (f(x)) Cons(x, Nil)
      else Nil
    )
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(as, Nil: List[A])(
      (xs, x) =>
        if (f(x)) Cons(x, xs)
        else xs
    )
  }

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, Nil: List[B])((xs, x) => Cons(f(x), xs))

  // Exercise 3.17
  def doubleToStringList(ds: List[Double]): List[String] = {
    foldLeft(ds, Nil: List[String])((as, b) => Cons(b.toString, as))
  }

  // Exercise 3.16
  def addOneEach(ns: List[Int]): List[Int] =
    foldLeft(ns, Nil: List[Int])((as, b) => Cons(b + 1, as))

  // Exercise 3.15
  def flatten[A](as: List[List[A]]): List[A] = {

    def loop(xs: List[List[A]]): List[A] = {
      xs match {
        case Nil => Nil
        case Cons(initList, tailLists) => append2(initList, loop(tailLists))
      }
    }

    loop(as)
  }

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight2(a1, a2)((a, bs) => Cons(a, bs))
  }

  // Exercise 3.13
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse2(as), z)((x, y) => f(y, x))
  }

  // Exercise 3.12
  def reverse[A](ns: List[A]): List[A] = {
    @tailrec
    def loop(xs: List[A], acc: List[A]): List[A] = {
      xs match {
        case Nil => acc
        case Cons(h, t) => loop(t, Cons(h, acc))
      }
    }

    loop(ns, Nil)
  }

  def reverse2[A](ns: List[A]): List[A] = {
    foldLeft(ns, Nil: List[A])((acc: List[A], x) => Cons(x, acc))
  }

  // Exercise 3.11
  def sum3(ns: List[Int]): Int = {
    foldLeft(ns, 0)((x, y) => x + y)
  }

  def product3(ns: List[Double]): Double = {
    foldLeft(ns, 1.0)(_ * _)
  }

  // Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(acc: B, ns: List[A]): B = {
      ns match {
        case Nil => acc
        case Cons(h, t) => loop(f(acc, h), t)
      }
    }

    loop(z, as)
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def init[A](l: List[A]): List[A] = {

    def loop(xs: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, loop(t))
      }
    }

    loop(l)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def loop(xs: List[A]): List[A] = {
      xs match {
        case Nil => Nil
        case Cons(head, tl) if f(head) => loop(tl)
        case _ => xs
      }
    }

    loop(l)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def loop(xs: List[A], i: Int): List[A] = {
      if (i < n) xs match {
        case Nil => Nil
        case Cons(_, tl) => loop(tl, i + 1)
      }
      else xs
    }

    loop(l, 0)
  }

  // Exercise 3.3
  def setHead[A](xs: List[A], h: A): List[A] = xs match {
    case Nil => List(h)
    case Cons(_, tl) => Cons(h, tl)
  }

  // Exercise 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, tl) => tl
  }

}
