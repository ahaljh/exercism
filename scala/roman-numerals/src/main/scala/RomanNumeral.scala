/**
  * Created by ahaljh on 2017. 2. 23..
  */
object RomanNumeral {
  def toNumerals(no: Int): String = {
    val numbers = List(no / 1000, no % 1000 / 100, no % 100 / 10, no % 10)
    val romans = List(("M", "", ""), ("C", "D", "M"), ("X", "L", "C"), ("I", "V", "X")) // (one, five, ten)

    numbers.zip(romans).map {
      case (i, (one, five, ten)) => {
        if (i < 4) one * i
        else if (i == 4) one + five
        else if (i < 9) five + one * (i - 5)
        else one + ten // 9
      }
    }.mkString

  }
}
