/**
  * Created by ahaljh on 2017. 4. 6..
  */
case class Matrix(matrixStr: String) {
  val matrix = matrixStr.split("\n").map(_.split(" ").map(_.toInt))

  def rows(r: Int): Seq[Int] = matrix(r)
  def cols(c: Int): Seq[Int] = matrix.map(arr => arr(c))
}
