/**
  * Created by ssatish on 2/11/18.
  */

object InsertionSort {
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  //alternate implementation
  def insertionSort[A <: Ordered[A]](list: List[A]): List[A] =
    list.foldLeft(List[A]()) { (r,c) =>
      val (front, back) = r.span(_ < c)  //span Returns a pair of lists - the longest prefix of the list whose elements all satisfy the given predicate, and the rest of the list.
      front ::: c :: back  // the ::: operator preprends a list to another list
    }
}

