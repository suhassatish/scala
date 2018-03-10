package week7

/**
  * Created by ssatish on 3/10/18.
  */
object week7 {
  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  /**
    * Sieve of Eratosthenes to find prime numbers. 1st find 2 and remove all multiple of it.
    * Then the 1st element left is a prime ie 3. Then remove all multiple of it.
    * At each step, remove multiple of the first element which is a prime and the remaining
    * elements will have their 1st element which is a prime. Implementation using Streams collection
    */
  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))

  val primes = sieve(from(2))

  primes.take(5).toList //to see the 1st 5 primes

  /**
    * With streams, we can express the concept of converging sequence without having to worry
    * about when to terminate it
    * @param x
    * @return
    */
  def sqrtStream(x: Double): Stream[Double] = {

    def improve(guess: Double) = (guess + x/guess)/2

    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)

    guesses
  }

  private def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess - x)/x ) < 0.0001

  sqrtStream(4).filter(isGoodEnough(_, 4))
}
