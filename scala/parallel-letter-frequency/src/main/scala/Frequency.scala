import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Frequency {
  private def splitSeq(splitNum: Int, seq: Seq[String]): Seq[Seq[Char]] = {
    if (seq.size == 0) Seq(Seq())
    else {
      val param = seq.size/splitNum + (if (seq.size%splitNum!=0) 1 else 0)

      seq.flatten.sliding(param, param).toSeq
    }
  }

  private def frequency(seq: Seq[Char]): Map[Char, Int] =
    seq.filter(_.isLetter).groupBy(_.toLower).mapValues(_.size)


  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] = {
    val futures = splitSeq(numWorkers, texts)
      .map(seq => Future{
        frequency(seq)
      })

    Await.result(Future.sequence(futures), Duration.Inf)
      .foldLeft(Map[Char,Int]()){ _ + _ }


  }
  implicit def mapToMyMap[A](m: Map[A, Int]): MyMap[A] = new MyMap[A](m)

}

class MyMap[A](m: Map[A,Int]) {
  def + (other: Map[A,Int]): Map[A,Int] = {
    (m.keySet ++ other.keySet)
      .toList
      .map{
        key => (key, m.getOrElse(key,0) + other.getOrElse(key,0))
      }.toMap
  }
}