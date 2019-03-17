package com.github.sdual.fpinscala.chap04

import scala.annotation.tailrec

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(x) => f(x)
      case None => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(x) => x
      case None => default
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case x@Some(_) => x
      case None => ob
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this.flatMap(x =>
      if (f(x)) this
      else None
    )
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

    def loop(a1: List[A], acc: List[B]): Option[List[B]] = {
      a match {
        case Nil => Some(acc)
        case x :: xs => f(x) match {
          case Some(y) => loop(xs, y :: acc)
          case None => None
        }
      }
    }

    loop(a, Nil).map(x => x.reverse)
  }

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    @tailrec
    def loop(a1: List[Option[A]], acc: List[A]): Option[List[A]] = {
      a1 match {
        case Nil => Some(acc)
        case x :: xs => x match {
          case None => None
          case Some(y) => loop(xs, y :: acc)
        }
      }
    }

    loop(a, Nil).map(x => x.reverse)
  }

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a1 => b.map(b1 => f(a1, b1)))
  }

  def abs0: Option[Double] => Option[Double] = lift(math.abs)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length.toDouble)
  }

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).map { m =>
      xs.map(x => math.pow(x - m, 2)).sum / xs.length.toDouble
    }
  }

}
