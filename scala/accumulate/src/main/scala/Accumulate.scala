/**
  * Created by ahaljh on 2017. 1. 23..
  */
class Accumulate {
  def accumulate[A, B](f: A=>B, l: List[A]): List[B] = {
    def process(list: List[A], acc: List[B]): List[B] = list match {
      case Nil => acc.reverse
      case h::t => process(t, f(h)::acc)
    }

    process(l, Nil)
  }

}
