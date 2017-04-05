import scala.annotation.tailrec

/**
  * Created by ahaljh on 2016. 11. 22..
  */
object Hamming {
  def compute(dna1: String, dna2: String): Option[Int] = {
    @tailrec
    def process(ls1: List[Char], ls2: List[Char], acc: Int): Option[Int] = (ls1, ls2) match {
      case (Nil, Nil) => Some(acc)
      case (h1::t1, h2::t2) => process(t1, t2, if (h1==h2) acc else acc+1)
      case _ => None
    }
    process(dna1.toList, dna2.toList, 0)
  }


}
