
case class Ocr(input: String) {
  def convert: String = {
    val line = input.split("\n").toList.grouped(4).toList

    line.map(convertOneLine).mkString(",")
  }

  // list has only 4 items
  private def convertOneLine(list: List[String]): String = {
    list.map(_.grouped(3).toList)
      .transpose
      .map(_.mkString("\n"))
      .map{ digitMap.getOrElse(_, "?") }
      .mkString
  }

  private val digitMap: Map[String, String] = Map(
    (List(" _ "
        , "| |"
        , "|_|"
        , "   ").mkString("\n"), "0"),
    (List("   "
        , "  |"
        , "  |"
        , "   ").mkString("\n"), "1"),
    (List(" _ "
        , " _|"
        , "|_ "
        , "   ").mkString("\n"), "2"),
    (List(" _ "
        , " _|"
        , " _|"
        , "   ").mkString("\n"), "3"),
    (List("   "
        , "|_|"
        , "  |"
        , "   ").mkString("\n"), "4"),
    (List(" _ "
        , "|_ "
        , " _|"
        , "   ").mkString("\n"), "5"),
    (List(" _ "
        , "|_ "
        , "|_|"
        , "   ").mkString("\n"), "6"),
    (List(" _ "
        , "  |"
        , "  |"
        , "   ").mkString("\n"), "7"),
    (List(" _ "
        , "|_|"
        , "|_|"
        , "   ").mkString("\n"), "8"),
    (List(" _ "
        , "|_|"
        , " _|"
        , "   ").mkString("\n"), "9")
  )
}
