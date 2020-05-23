package com.github.sdual.fpinscala.chap03

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.28
  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = {

    def loop(tr: Tree[A]): Tree[B] = {
      tr match {
        case Branch(l, r) => Branch(loop(l), loop(r))
        case Leaf(v) => Leaf(f(v))
      }
    }

    loop(tree)
  }

  // Exercise 3.27
  def depth[A](tree: Tree[A]): Int = {
    def loop(tr: Tree[A], d: Int): Int = {
      tr match {
        case Branch(l, r) => loop(l, d + 1) max loop(r, d + 1)
        case Leaf(_) => d + 1
      }
    }

    loop(tree, 0)
  }

  // Exercise 3.26
  def maximum(tree: Tree[Int]): Int = {

    def loop(tr: Tree[Int]): Int = {
      tr match {
        case Branch(l, r) => loop(l) max loop(r)
        case Leaf(v)      => v
      }
    }

    loop(tree)
  }

  // Exercise 3.25
  def size[A](tree: Tree[A]): Int = {

    def loop(tr: Tree[A]): Int = {
      tr match {
        case Branch(l, r) => loop(l) + loop(r)
        case Leaf(_) => 1
      }
    }

    loop(tree)
  }

}
