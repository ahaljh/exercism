
object PascalsTriangle {
  def triangle(depth: Int): List[List[Int]] = {
    def process(d: Int, prev: List[Int], acc: List[List[Int]]): List[List[Int]] = {
      val nextLine = next(prev)
      val nextAcc = nextLine :: acc

      if (d == depth) nextAcc.reverse
      else process(d+1, nextLine, nextAcc)
    }

    def next(prev: List[Int]): List[Int] = {
      if (prev == Nil) List(1)
      else ((0 :: prev) :+ 0).sliding(2, 1).map(_.sum).toList
    }

    process(1, Nil, Nil)
  }

}
