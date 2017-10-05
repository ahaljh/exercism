import scala.annotation.tailrec

object PigLatin {
  val vowels = List('a','e','i','o','u')

  def translate(sentence: String): String = {
    def translateWord(word: String): String = {
      @tailrec
      def divide(s: String): String = {
        if (s.startsWith("qu")) divide(s.substring(2) + "qu")
        else if (!vowels.contains(s.charAt(0))) divide(s.substring(1) + s.charAt(0))
        else s
      }

      divide(word) +"ay!"
    }

    sentence.split(" ")
      .map(translateWord(_))
      .mkString(" ")
  }



}
