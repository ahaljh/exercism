import Bearing.Bearing

/**
  * Created by ahaljh on 2017. 3. 19..
  */
case class Robot(bearing: Bearing, coordinates: (Int, Int)) {
  def advance: Robot = bearing match {
    case Bearing.North => Robot(bearing, (coordinates._1, coordinates._2+1))
    case Bearing.East => Robot(bearing, (coordinates._1+1, coordinates._2))
    case Bearing.South => Robot(bearing, (coordinates._1, coordinates._2-1))
    case Bearing.West => Robot(bearing, (coordinates._1-1, coordinates._2))
  }

  def turnRight() = Robot(Bearing.rightDirection(bearing), coordinates)

  def turnLeft() = Robot(Bearing.leftDirection(bearing), coordinates)

  def simulate(str: String) = {
    str.foldLeft(this){
      case (robot, c) => c match {
        case 'R' => robot.turnRight()
        case 'L' => robot.turnLeft()
        case 'A' => robot.advance
      }
    }
  }
}


final object Bearing extends Enumeration {
  type Bearing = Value
  val North, East, South, West = Value

  val rightDirection = Map(North -> East, East -> South, South -> West, West -> North)
  val leftDirection = rightDirection.map{ case (key, value) => (value, key) }
}