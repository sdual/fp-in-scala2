package com.github.sdual.fpinscala.chap02

import scala.annotation.tailrec
import scala.math.abs

object Main1 extends App {

  println(isSorted(Array(1, 2, 4, 4), (x: Int, y: Int) => x <= y))

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => f(a, _)
  }

  def curry2[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (i + 1 >= as.length) true
      else if (ordered(as(i), as(i + 1))) loop(i + 1)
      else false
    }

    loop(0)
  }

  // polymorphic function. generic function.
  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  // higher order function.
  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatFactorial(n: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  // Exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def loop(i: Int, a1: Int, a2: Int): Int = {
      if (i < n) loop(i + 1, a2, a1 + a2)
      else a1
    }

    loop(0, 0, 1)
  }

}
