/**
  * Created by ahaljh on 2016. 12. 16..
  */
case class Year(year: Int) {
  def isLeap = (isDivided(400) || (isDivided(4) && !isDivided(100)) )

  private def isDivided(divisor: Int) = year % divisor == 0
}
