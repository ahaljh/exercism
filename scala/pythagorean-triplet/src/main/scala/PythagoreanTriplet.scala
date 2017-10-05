/**
  * Created by ahaljh on 2017. 5. 22..
  */
object PythagoreanTriplet {
  def isPythagorean(tuple: (Int, Int, Int)): Boolean = {
    val (longest, others) = divideLines(tuple)

    isPythagorean(longest, others)
  }

  def pythagoreanTriplets(begin: Int, end: Int) = {
    for {
      a <- begin until end
      b <- a to end
      c <- b to end
      if isPythagorean(c, (a, b))
    } yield (a, b, c)
  }

  private def divideLines(tuple3: Tuple3[Int, Int, Int]): (Int, (Int, Int)) =  tuple3 match {
    case (a, b, c) if (a >= b && a >= c) => (a, (b, c))
    case (a, b, c) if (b >= a && b >= c) => (b, (a, c))
    case (a, b, c) => (c, (a, b))
  }

  private def isPythagorean(longest: Int, others: (Int, Int)): Boolean =
    longest * longest == others._1 * others._1 + others._2 * others._2
}
