object Acronym {
  def abbreviate(phrase: String): String = {
    phrase.takeWhile(_ != ':')
      .split("[-, ]+")
      .map(_.toCharArray.toList match {
        case h::t => h.toString.toUpperCase + t.filter(_.isUpper).mkString
        case Nil => ""
      }).mkString
  }
}
