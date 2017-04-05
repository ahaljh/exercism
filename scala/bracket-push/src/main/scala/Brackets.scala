/**
  * Created by ahaljh on 2017. 3. 23..
  */
object Brackets {

  private val brackets = "(){}[]"
  private val matchedBrackets = List("()", "{}", "[]")

  def areBalanced(sentence: String): Boolean = {
    def removeMatchedBracktes(str: String): String = {
      if (!matchedBrackets.exists{ str.contains _ }) str
      else {
        val removedStr = matchedBrackets.foldLeft(str){ (s, bracket) => { s.replace(bracket, ""); } }

        removeMatchedBracktes(removedStr)
      }
    }

    removeMatchedBracktes{ sentence.filter(brackets contains _) }.isEmpty
  }

}
