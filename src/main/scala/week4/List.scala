package week4

import objsets.{EmptySet, NonEmptySet}

/**
  * Created by ssatish on 2/11/18.
  */
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T]() = new Nil
}

object test {
  def f(xs: List[NonEmptySet], x: EmptySet) = xs.prepend(x)
}

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

object exprs {
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
  }
}
