/**
  * Created by ahaljh on 2016. 12. 2..
  */
class PhoneNumber(phoneNumberStr: String) {
  def number :Option[String] = {
    val onlyNumber = phoneNumberStr.filter(_.isDigit)
    if (onlyNumber.length == 10) Some(onlyNumber)
    else if (onlyNumber.length == 11 && onlyNumber.startsWith("1")) Some(onlyNumber.substring(1))
    else None
  }

  def areaCode :Option[String] = {
    number.map(_.take(3))
  }

  def prettyPrint: Option[String] = {
    number.map(s => s"(${s.take(3)}) ${s.substring(3,6)}-${s.substring(6)}")
  }

}
