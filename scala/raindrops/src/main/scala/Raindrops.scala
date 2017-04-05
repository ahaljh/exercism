object Raindrops {
  def convert(n: Int): String = {
    val retStr: String = List((3, "Pling"), (5, "Plang"), (7, "Plong"))
      .map{ case (i, s) => if (n%i == 0) s else "" }
      .mkString

    if (retStr.isEmpty) n.toString
    else retStr
  }
}

