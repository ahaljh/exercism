object CryptoSquare {
  def normalizePlaintext(text: String): String =
    text.filter(_.isLetterOrDigit).toLowerCase

  def squareSize(text: String): Int = {
    math.ceil(math.sqrt(text.length)).toInt
  }

  def plaintextSegments(text: String): List[String] = {
    val normalizedText = normalizePlaintext(text)

    if (normalizedText.isEmpty) Nil
    else normalizedText.grouped(squareSize(normalizedText)).toList
  }

  def ciphertext(text: String): String = {
    ciphertextlist(text).mkString
  }

  def normalizedCiphertext(text: String): String = {
    ciphertextlist(text).mkString(" ")
  }

  private def ciphertextlist(text: String): List[String] = {
    def process(textList: List[String], acc: List[String]): List[String] = {
      if (textList.isEmpty) acc.reverse
      else {
        val heads = textList.map(_.take(1)).mkString
        val tails = textList.map(_.drop(1)).filterNot(_.isEmpty)

        process(tails, heads :: acc)
      }
    }

    process(plaintextSegments(text), Nil)
  }
}
