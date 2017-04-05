import scala.annotation.tailrec

object Strain {
  def keep[T](seq: Seq[T], f: T => Boolean) =
    process(seq.toList.reverse, f, true, Nil)

  def discard[T](seq: Seq[T], f: T => Boolean) =
    process(seq.toList.reverse, f, false, Nil)


  @tailrec
  private def process[T](list: List[T], fun: T => Boolean, cond: Boolean, acc: List[T]): List[T] = list match {
    case Nil => acc
    case h::t => process(t, fun, cond, if (fun(h) == cond) h::acc else acc)
  }
}