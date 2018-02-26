package week6
/**
  * Created by ssatish on 2/19/18.
  */


object week6 {
  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map{case (x, y) => x * y}.sum

  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)  //`until` goes upto n - 1; `to` includes n

  /* Given n, find all pairs (i,j) each < n such that (i+j) is prime. Eg, if n = 6
    i     2  3  4  4  5  6  6
    j     1  2  1  3  2  1  5
  i + j   3  5  5  7  7  7  11

  def isSumPrime(n: Int): Boolean = {
    (1 until n) flatmap (i =>
      (1 until i).map ( j => (i, j))) filter ( pair =>
        isPrime(pair._1 + pair._2))

  }
  */
  //----------------------------------------------------------------------------------
  def queens(n: Int) = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
      } yield col :: queens
    }
    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1).zip(queens)
    queensWithRow.forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines.mkString("\n"))
  }
  queens(4).map(show).mkString("\n")
  //----------------------------------------------------------------------------------
  case class Book(title: String, authors: List[String])  // title, List[authors]
  val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs", authors = List("Abelson, Herald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programing", authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java", authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers", authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala", authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  //to find titles of books where author's name is "Bird"
  for (b <- books; a <- b.authors if a.startsWith("Bird,"))
    yield b.title

  //above can be re-written as below (according to Odersky's lecture solution), but it doesn't return equivalent result!
  books.flatMap(b =>
    b.authors.withFilter(a => a startsWith "Bird,").map(y => y))



  //find all books that have the word "program" in the title
  for (b <- books if b.title.indexOf("Program") >= 0)
    yield b.title

  //find all authors who have written atleast 2 books
  {
    for {
      b1 <- books
      b2 <- books
      if b1.title < b2.title
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1
  }.distinct
//----------------------------------------------------------------------------------
  for {
    i <- 1 until 10
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

  //above expression can be written in terms of partial functions map & flat-map as below
  (1 until 10).flatMap(i =>
    (1 until i).withFilter(j => isPrime(i + j))
      .map(j => (i, j)))

  //----------------------------------------------------------------------------------
  /*
  val e1 = List()
  val e2 = List()
  val s = Seq()
  val e3 = Vector()

  for(x <- e1; y <- e2; s ) yield e3

  //above is equivalent to
  e1.flatMap(x => for (y <- e2; s) yield e3)
  */
  //----------------------------------------------------------------------------------
  //Map collection type querying
  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

  capitalOfCountry("Andorra")  //throws java.util.NoSuchElementException: key not found

  //to query a map without knowing beforehand whither it contains a given key, use the get operation
  capitalOfCountry.get("Andorra") // None
  capitalOfCountry.get("Us") // Some("Washington")
  //----------------------------------------------------------------------------------
  // DEFINITION OF OPTION AS CASE CLASSES
  /*
  trait Option[+A]
  case class Some[+A](value: A) extends Option[A]
  object None extends Option[Nothing]
  */

  //since Option is defined as case classes, they can be decomposed using pattern matching

  def showCapital(country: String) = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "Missing data"
  }

  val cap1 = capitalOfCountry.withDefaultValue("<unknown>")
  cap1("Andorra")  // "<unknown>"
  
  //----------------------------------------------------------------------------------

  //OrderBy in sql can be implemented on a scala collection using `sortWith` and `sorted`
  val fruit = List("apple", "pear", "orange", "pineapple")
  fruit.sortWith(_.length < _.length) // List("pear", "apple", "orange", "pineapple")
  fruit.sorted //List("apple", "orange", "pear", "pineapple")

  //groupBy returns a Map
  fruit.groupBy(_.head)
  /* Map(p -> List(pear, pineapple), )
         a -> List(apple),
         o -> List(orange))
   */

  //a polynomial like x^3 -2x + 5 is a Map of exponents to coefficients
  Map(0 -> 5, 1 -> 2, 3 -> 1)

  // the number of key -> value pairs passed to Map can vary. So we use the `repeated parameter` binding as follows.
  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)  //inside the fn, bindings are seen as Seq[(Int, Double)]

    val terms = terms0.withDefaultValue(0.0)

    //def +(other: Poly) = new Poly(terms ++ (other.terms.map(adjust)))  //naive implementation
    def +(other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))  //more efficient as avoids intermediate List
    // data structure

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse)
        yield coeff + "x^" + exp).mkString(" + ")
  }

}
