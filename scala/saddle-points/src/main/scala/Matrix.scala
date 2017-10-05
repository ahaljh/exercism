
case class Matrix(matrix: List[List[Int]]) {
  def saddlePoints: Set[(Int, Int)] = {
    val pointOfMaxRow = for {
      (list, row) <- matrix.zipWithIndex
      (value, col) <- list.zipWithIndex if (value == list.max)
    } yield (row, col)

    val pointOfMinCol = for {
      (list, col) <- matrix.transpose.zipWithIndex
      (value, row) <- list.zipWithIndex if (value == list.min)
    } yield (row, col)

    pointOfMaxRow.toSet.intersect(pointOfMinCol.toSet)
  }

}
