import java.util.{Calendar, GregorianCalendar}

/**
  * Created by ahaljh on 2016. 12. 18..
  */
case class Meetup(month: Int, year: Int) {

  def teenth(weekday: Int) = getDate(13, 19, weekday)

  def first(weekday: Int) = getDate(1, 7, weekday)

  def second(weekday: Int) = getDate(8, 14, weekday)

  def third(weekday: Int) = getDate(15, 21, weekday)

  def fourth(weekday: Int) = getDate(22, 28, weekday)

  def last(weekday: Int) = {
    def getLastDayOfMonth(year: Int, month: Int): Int = {
      val calendar = Calendar.getInstance()
      calendar.set(year, month-1, 1)
      calendar.getActualMaximum(Calendar.DAY_OF_MONTH)
    }

    val lastDay = getLastDayOfMonth(year, month)
    getDate(lastDay-6, lastDay, weekday)
  }


  private def getDate(fromDay: Int, toDay: Int, weekday: Int): GregorianCalendar = {
    (fromDay to toDay)
      .map(new GregorianCalendar(year, month-1, _))
      .find(_.get(Calendar.DAY_OF_WEEK) == weekday)
      .get
  }

}

object Meetup {
  val Sun = 1
  val Mon = 2
  val Tue = 3
  val Wed = 4
  val Thu = 5
  val Fri = 6
  val Sat = 7
}
