/**
  * Created by ahaljh on 2016. 11. 24..
  */
class Phrase(str: String) {
  def wordCount = {
    str.toLowerCase
      .filterNot("!@#$%^&*:.".contains(_))
      .split("[ ,]+")
      .groupBy(identity)
      .mapValues(_.size)
  }

}
