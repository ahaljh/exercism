
case class Point(x: Int, y: Int) {
  import Direction._

  def move(direction: Direction): Point = direction match {
    case Left => Point(x-1, y)
    case Right => Point(x+1, y)
    case Up => Point(x, y-1)
    case Down => Point(x, y+1)
  }
}

object SpiralMatrix {
  import Direction._

  def spiralMatrix(size: Int): List[List[Int]] = {
    def makeList(cur: Point, no: Int, direction: Direction, acc: List[(Point, Int)], numbered: Map[Point, Boolean]): List[(Point, Int)] = {
      if (no > size*size) acc
      else if (no == size*size) (cur,no)::acc
      else {
        val next: Point = cur.move(direction)

        if (numbered.getOrElse(next, true)) makeList(cur, no, Direction.rotate(direction), acc, numbered)
        else makeList(next, no+1, direction, (cur,no)::acc, numbered + (cur -> true))
      }
    }

    val defaultOccupied = for {
      x <- 1 to size
      y <- 1 to size
    } yield (Point(x,y), false)

    val newList = makeList(Point(1,1), 1, Right, Nil, defaultOccupied.toMap)

    newList.groupBy{ case (p: Point, i: Int) => p.y }
      .toList
      .sortWith(_._1 < _._1)
      .map(_._2)
      .map{ case (list: List[(Point, Int)]) => list.sortWith(_._1.x < _._1.x).map(_._2) }
  }
}

object Direction extends Enumeration {
  type Direction = Value

  val Left, Right, Up, Down = Value

  def rotate(direction: Direction): Direction = direction match {
    case Left => Up
    case Right => Down
    case Up => Right
    case Down => Left
  }
}
