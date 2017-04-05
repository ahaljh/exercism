/**
  * Created by ahaljh on 2016. 11. 21..
  */
class Bob {
  def hey(sentence: String): String = {
    if (sentence.exists(_.isLetter) && sentence.forall(!_.isLower)) "Whoa, chill out!"
    else if (sentence.endsWith("?")) "Sure."
    else if (sentence.trim.isEmpty) "Fine. Be that way!"
    else "Whatever."
  }
}
