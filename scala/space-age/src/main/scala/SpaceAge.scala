/**
  * Created by ahaljh on 2016. 12. 19..
  */
case class SpaceAge(val seconds: Long) {
  private val EARTH_YEAR_SECONDS = 31557600

  private def toYear(rate: Double): Double =
    math.round(seconds.toDouble * 100 / rate / EARTH_YEAR_SECONDS) / 100.0

  def onEarth = toYear(1)

  def onMercury = toYear(0.2408467)

  def onVenus = toYear(0.61519726)

  def onMars = toYear(1.8808158)

  def onJupiter = toYear(11.862615)

  def onSaturn = toYear(29.447498)

  def onUranus = toYear(84.016846)

  def onNeptune = toYear(164.79132)
}
