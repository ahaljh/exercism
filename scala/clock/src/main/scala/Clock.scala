class Clock(inputHour: Int, inputMinute: Int) {
  // convert (-) value to (+)
  private val plusHour = if (inputHour > 0) inputHour else inputHour%24 + 24
  private val plusMinute = if (inputMinute > 0) inputMinute else inputMinute%(24*60) + (24*60)

  val hour: Int = (plusHour + plusMinute / 60) % 24
  val minute: Int = plusMinute % 60

  override def toString: String = "%02d:%02d".format(hour, minute)

  override def equals(other: Any): Boolean = other match {
    case that: Clock => (this.hour == that.hour) && (this.minute == that.minute)
    case _ => false
  }

  def +(other: Clock): Clock = {
    new Clock(this.hour + other.hour, this.minute + other.minute)
  }

  def -(other: Clock): Clock = {
    new Clock(this.hour - other.hour, this.minute - other.minute)
  }
}

object Clock {
  def apply(hour: Int, minute: Int) = new Clock(hour, minute)
  def apply(minutes: Int): Clock = Clock(minutes/60, minutes%60)
}