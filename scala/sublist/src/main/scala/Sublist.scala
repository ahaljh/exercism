/**
  * Created by ahaljh on 2017. 1. 23..
  */
object Sublist extends Enumeration{
  val Equal, Sublist, Superlist, Unequal = Value

  def sublist[A](la: List[A], lb: List[A]) = {
    val aIsSub = isSubList(la, lb)
    val bIsSub = isSubList(lb, la)

    if (aIsSub && bIsSub) Equal
    else if (aIsSub) Sublist
    else if (bIsSub) Superlist
    else Unequal
  }

  private def isSubList[A](sub: List[A], main: List[A]): Boolean = (sub, main) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (_, h::t) => main.startsWith(sub) || isSubList(sub, t)
  }
}
