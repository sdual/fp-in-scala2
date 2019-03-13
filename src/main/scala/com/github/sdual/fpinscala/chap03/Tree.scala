package com.github.sdual.fpinscala.chap03

trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.26
  //def maximun(tree: Tree[Int]): Int = {

//    def loop(left: Tree[Int], right: Tree[Int], tmpMax: Int): Int = {
//      val leftMax = left match {
//        case Branch(l, r) => loop(r, l, tmpMax)
//        case Leaf(v)      => math.max(v, tmpMax)
//      }
//
//      val rightMax = right match {
//        case Branch(l, r) => loop(r, l, tmpMax)
//        case Leaf(v)      => math.max(v, tmpMax)
//      }
//
//      math.max(leftMax, rightMax)
//    }
//
//
//  }

  // Exercise 3.25
  def size[A](tree: Tree[A]): Int = {

    def loop(left1: Tree[A], right1: Tree[A], nodeNum: Int): Int = {
      val leftNum = left1 match {
        case Branch(l, r) => loop(r, l, nodeNum + 1)
        case Leaf(_)      => nodeNum + 1
      }

      val rightNum = right1 match {
        case Branch(l, r) => loop(r, l, nodeNum + 1)
        case Leaf(_)      => nodeNum + 1
      }

      leftNum + rightNum
    }

    tree match {
      case Branch(left, right) => loop(left, right, 1)
      case _ => 1
    }
  }

}
