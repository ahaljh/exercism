object RunLengthEncoding {

  def encode(str: String): String = {
    def process(l: List[Char], acc: String): String = {
      val (char, len, subList) = chk(l)
      if (len == 0) acc
      else process(subList, acc + (if (len > 1) len else "") + char)
    }

    def chk(l: List[Char]): (Char, Int, List[Char]) = l match {
      case Nil => (' ', 0, Nil)
      case h::t => (h, l.takeWhile(_==h).size, l.dropWhile(_==h))
    }

    process(str.toList, "")
  }

  def decode(str: String): String = {
    def process(l: List[Char], acc: String): String = l match {
      case Nil => acc
      case _ => {
        val (i, s, subL) = pickOne(l, "")
        process(subL, acc + s*i)
      }
    }

    def pickOne(l: List[Char], digit: String): (Int, String, List[Char]) = l match {
      case Nil => (0, "", Nil)
      case h::t if h.isDigit => pickOne(t, digit+h)
      case h::t => (if (digit.isEmpty) 1 else digit.toInt, h.toString, t)
    }

    process(str.toList, "")
  }
}
