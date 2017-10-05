
object Series {
  def slices(size: Int, str: String) = {
    val intList = str.toCharArray.toList.map(_.asDigit)

    for (i <- (0 to intList.length-size).toList)
      yield intList.slice(i, i+size)
  }

}
