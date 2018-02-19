/**
  * Created by ssatish on 2/10/18.
  */
package week4

/**
  * Non-negative natural number. Peano numbers: Starting from natural numbers, constructs floating point numbers.
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
  def successor = new Succ(this)
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def +(that: Nat) = that
  def -(that: Nat) = if (that.isZero) this else throw new Error("negative numbers")
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def +(that: Nat) = new Succ(n + that)
  def -(that: Nat) = if (that.isZero) this else n - that.predecessor
}

