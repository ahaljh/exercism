object Queens {

  def boardString(white: Option[Position], black: Option[Position]): String = {
    val boardArr = Array.fill(8,8) {'_'}

    white.map(position => boardArr(position.x)(position.y) = 'W')
    black.map(position => boardArr(position.x)(position.y) = 'B')

    boardArr.map(line => line.mkString(" ") + "\n").mkString
  }

  def canAttack(white: Position, black: Position): Boolean =
    (white.x == black.x || white.y == black.y
      || (white.y-white.x) == (black.y-black.x)
      || (white.x+white.y) == (black.x+black.y))

}

case class Position(x: Int, y: Int)
