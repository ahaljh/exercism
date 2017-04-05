/**
  * Created by ahaljh on 2016. 11. 27..
  */
class Anagram(word: String) {
  val sortedWord = word.toLowerCase.sorted

  def matches(strings: Seq[String]) = {
    strings.filter { s =>
      !s.equalsIgnoreCase(word) && s.toLowerCase.sorted == sortedWord
    }
  }

}
