/**
  * Created by ahaljh on 2017. 1. 30..
  */
object Dna {
  private val conversionMap = Map(("G" -> "C"), ("C" -> "G"), ("T" -> "A"), ("A" -> "U"))

  def toRna(dnaStr: String): Option[String] = {
    dnaStr.foldLeft(Option(""))((b: Option[String], a: Char) => concat(b, conversionMap.get(a.toString)))
  }

  private def concat(os1: Option[String], os2: Option[String]) = (os1, os2) match {
    case (_, None) | (None, _) => None
    case (Some(s1), Some(s2)) => Some(s1+s2)
  }

}
