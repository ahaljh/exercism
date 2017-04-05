/**
  * Created by ahaljh on 2016. 11. 27..
  */
object Pangrams {
  def isPangram(s: String):Boolean = {
    ('a' to 'z').forall(s.toLowerCase().contains(_))
  }
}
