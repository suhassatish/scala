package recfun

/**
  * Created by ssatish on 2/10/18.
  */

abstract class IntSet {
  def incl(x: Int): IntSet  // include an element x into IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

/**
  * Implementation of EmptySet
  */
class EmptySet extends IntSet {
  def contains(x: Int): Boolean = false

  /**
    * Once you include an element x into an empty set, it becomes a non-empty set
    * @param x
    * @return
    */
  def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)

  override def toString = "."

  def union(other: IntSet): IntSet = other
}

/**
  * Constructor parameters to a class can be specified as function arguments
  * to the class as shown below.
  * NonEmptySet is implemented as a TreeSet with a LeftSubtreeSet and RightSubtreeSet
  */
class NonEmptySet(elem: Int, leftSet: IntSet, rightSet: IntSet) extends IntSet {

  def contains(x: Int): Boolean =
    //binary search for elements in TreeSet until element x is found
    // in the terminal case, it calls EmptySet.contains(x) and returns false.
    if (x < elem) leftSet.contains(x)
    else if (x > elem) rightSet.contains(x)
    else true

  def incl(x: Int): IntSet =
    if(x < elem) new NonEmptySet(elem, leftSet.incl(x), rightSet)
    else if (x > elem) new NonEmptySet(elem, leftSet, rightSet.incl(x))
    else this

  override def toString = "{" + leftSet + elem + rightSet + "}"

  def union(other: IntSet): IntSet =
    leftSet.union(rightSet).union(other).incl(elem)
}
