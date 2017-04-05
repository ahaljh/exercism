/**
  * Created by ahaljh on 2017. 4. 2..
  */
object Series {
  def largestProduct(series: Int, number: String): Option[Int] = {
    if (series == 0) Some(1)
    else if (number.length < series) None
    else Some {
      number.map(_.asDigit)
        .sliding(series, 1)
        .map(_.product)
        .max
    }
  }

}
