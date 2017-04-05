import scala.annotation.tailrec

/**
  * Created by ahaljh on 2016. 12. 2..
  */
class DNA(dnaString: String) {
  val nucleotideCounts: Either[String, Map[Char, Int]] = {
    dnaString
      .toList
      .foldLeft(Right(Map('A'->0, 'C'->0, 'G'->0, 'T'->0)): Either[String, Map[Char, Int]]) {
        (nMap: Either[String, Map[Char, Int]], char: Char) =>
          if (!"ACGT".contains(char)) Left(s"invalid nucleotide '${char}'")
          else nMap.right.map(mm => mm.updated(char, mm(char)+1))
      }
  }

  def count(nucleotide: Char): Either[String, Int] = {
    if (!"ACGT".contains(nucleotide)) Left(s"invalid nucleotide '${nucleotide}'")
    else nucleotideCounts.right.map(m => m(nucleotide))

  }

}
