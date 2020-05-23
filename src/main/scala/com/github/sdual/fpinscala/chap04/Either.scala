package com.github.sdual.fpinscala.chap04

sealed trait Either[+E, +A] {

  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(right) => Right(f(right))
      case left@Left(_) => left
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(right) => f(right)
      case left@Left(_) => left
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case right@Right(_) => right
      case Left(_)        => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case Right(right1) => b match {
        case Right(right2) => Right(f(right1, right2))
        case left2@Left(_) => left2
      }
      case left1@Left(_) => left1
    }
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)
  }

}
