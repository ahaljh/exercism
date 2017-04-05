/**
  * Created by ahaljh on 2017. 1. 23..
  */
class Scrabble {
  //import Scrabble._
  def scoreLetter(c: Char) = scores(c.toUpper)

  def scoreWord(s: String) =
    s.map(scoreLetter(_)).sum
}

object Scrabble {
  val l = List(("AEIOULNRST", 1), ("DG", 2), ("BCMP", 3), ("FHVWY", 4), ("K", 5), ("JX", 8), ("QZ", 10))
  val scores = l.flatMap{case (s: String, i: Int) => s.map((c: Char) => (c, i))}.toMap
}
