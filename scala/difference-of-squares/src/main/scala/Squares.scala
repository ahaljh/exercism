/**
  * Created by ahaljh on 2017. 1. 23..
  */
object Squares {
  def squareOfSums(i: Int) = {
    val sum = (1 to i).sum
    sum * sum
  }


  def sumOfSquares(i: Int) =
    (1 to i).map(i => i*i).sum

  def difference(i: Int) =
    squareOfSums(i) - sumOfSquares(i)
}
