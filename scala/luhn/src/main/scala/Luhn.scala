/**
  * Created by ahaljh on 2017. 4. 3..
  */
object Luhn {
  def validate(number: String): Boolean = {
    val trimmedNumber = number.filterNot(_.isWhitespace)

    if (trimmedNumber.exists(!_.isDigit)) false
    else if (trimmedNumber.length == 1) false
    else {
      val luhnSum = trimmedNumber
        .map(_ - '0').reverse
        .zipWithIndex
        .map{ case (value, index) =>
          if (index%2 == 0) value
          else if (value * 2 > 9) value * 2 - 9
          else value * 2 }
        .sum

      (luhnSum % 10 == 0)
    }
  }

}
