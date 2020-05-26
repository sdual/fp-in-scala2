package com.github.sdual.fpinscala.chap05

object Main extends App {

  // non-strictness or laziness
//  val result = List(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)
//  println(result)
//
//  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = {
//    if (cond) onTrue() else onFalse()
//  }
//
//  val a = 11
//  // () => A thunk
//  if2(a < 22, () => println("a"), () => println("b"))

  val stream1 = Stream(1, 2, 3, 4)

  println(stream1.toList)

}
