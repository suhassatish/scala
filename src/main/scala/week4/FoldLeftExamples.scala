/**
  * Created by ssatish on 2/19/18.
  * taken from http://oldfashionedsoftware.com/2009/07/30/lots-and-lots-of-foldleft-examples/
  */

object FoldLeftExamples {
  def average2(list: List[Double]): Double =
    list.foldLeft(0.0)(_+_) / list.foldLeft(0.0)((r,c) => r+1)

  def average(list: List[Double]): Double = list match {
    case head :: tail => tail.foldLeft( (head,1.0) )((r,c) =>
      ((r._1 + (c/r._2)) * r._2 / (r._2+1), r._2+1) )._1
    case Nil => Double.NaN
  }

  def count(list: List[Any]): Int =
    list.foldLeft(0)((sum,_) => sum + 1)

  def product(list: List[Int]): Int = list.foldLeft(1)(_*_)

  def sum(list: List[Int]): Int = list.foldLeft(0)((r,c) => r+c)
  def sum2(list: List[Int]): Int = list.foldLeft(0)(_+_)

  def last[A](list: List[A]): A =
    list.foldLeft[A](list.head)((_, c) => c)

  def penultimate[A](list: List[A]): A =
    list.foldLeft( (list.head, list.tail.head) )((r, c) => (r._2, c) )._1

  def contains[A](list: List[A], item: A): Boolean =
    list.foldLeft(false)(_ || _==item)

  def get[A](list: List[A], idx: Int): A =
    list.tail.foldLeft((list.head,0)) {
      (r,c) => if (r._2 == idx) r else (c,r._2+1)
    } match {
      case (result, index) if (idx == index) => result
      case _ => throw new Exception("Bad index")
    }

  def mimicToString[A](list: List[A]): String = list match {
    case head :: tail => tail.foldLeft("List(" + head)(_ + ", " + _) + ")"
    case Nil => "List()"
  }

  def reverse[A](list: List[A]): List[A] =
    list.foldLeft(List[A]())((r,c) => c :: r)

  def unique[A](list: List[A]): List[A] =
    list.foldLeft(List[A]()) { (r,c) =>
      if (r.contains(c)) r else c :: r
    }.reverse

  def unique2[A](list: List[A]): List[A] =
    list.foldLeft(List[A]()) { (r,c) =>
      if (r.contains(c)) r else c :: r
    }.foldLeft(List[A]())((r,c) => c :: r)

  def toSet[A](list: List[A]): Set[A] =
    list.foldLeft(Set[A]())( (r,c) => r + c)

  def double[A](list: List[A]): List[A] =
    list.foldLeft(List[A]())((r,c) => c :: c :: r).reverse

  def double2[A](list: List[A]): List[A] =
    list.foldRight(List[A]())((c,r) => c :: c :: r)

  def pivot[A <: Ordered[A]](list: List[A]): (List[A],A,List[A]) =
    list.tail.foldLeft[(List[A],A,List[A])]( (Nil, list.head, Nil) ) {
      (result, item) =>
        val (r1, pivot, r2) = result
        if (item < pivot) (item :: r1, pivot, r2) else (r1, pivot, item :: r2)
    }

  def quicksort[A <: Ordered[A]](list: List[A]): List[A] = list match {
    case head :: _ :: _ =>
      println(list)
      list.foldLeft[(List[A],List[A],List[A])]( (Nil, Nil, Nil) ) {
        (result, item) =>
          val (r1, r2, r3) = result
          if      (item < head) (item :: r1, r2, r3)
          else if (item > head) (r1, r2, item :: r3)
          else                  (r1, item :: r2, r3)
      } match {
        case (list1, list2, list3) =>
          quicksort(list1) ::: list2  ::: quicksort(list3)
      }
    case _ => list
  }

  def encode[A](list: List[A]): List[(A,Int)] =
    list.foldLeft(List[(A,Int)]()){ (r,c) =>
      r match {
        case (value, count) :: tail =>
          if (value == c) (c, count+1) :: tail
          else            (c, 1) :: r
        case Nil =>
          (c, 1) :: r
      }
    }.reverse

  def decode[A](list: List[(A,Int)]): List[A] =
    list.foldLeft(List[A]()){ (r,c) =>
      var result = r
      for (_ <- 1 to c._2) result = c._1 :: result
      result
    }.reverse

  def group[A](list: List[A], size: Int): List[List[A]] =
    list.foldLeft( (List[List[A]](),0) ) { (r,c) => r match {
      case (head :: tail, num) =>
        if (num < size)  ( (c :: head) :: tail , num + 1 )
        else             ( List(c) :: head :: tail , 1 )
      case (Nil, num) => (List(List(c)), 1)
    }
    }._1.foldLeft(List[List[A]]())( (r,c) => c.reverse :: r)
}


