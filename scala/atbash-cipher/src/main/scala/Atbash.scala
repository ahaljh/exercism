object Atbash {
  def encode(sentence: String): String = {
    val alphabets = ('a' to 'z')
    val digits = ('0' to '9')
    val keyMap = (alphabets.zip(alphabets.reverse) ++ digits.zip(digits)).toMap

    sentence.filter(_.isLetterOrDigit).toLowerCase.map(keyMap)
      .grouped(5).mkString(" ")
  }
}
