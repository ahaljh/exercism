
sealed trait Bowling {
  def roll(pins: Int): Bowling

  def score(): Either[Error, Int]
}

object Bowling {
  def apply(): Bowling = ProcessingGame((1,1), 0, 10, 0, 0)
}

case class ProcessingGame(curTurn: (Int, Int), curScore: Int, remainPins: Int, doubleNextTwo: Int, doubleNextOne: Int) extends Bowling {
  override def roll(pins: Int): Bowling = {
    if (pins < 0) ErrorGame(Error("score cannot be negative."))
    else if (pins > remainPins) ErrorGame(Error("score cannot be more than pins"))
    else {
      // Check this roll is last roll.
      val isLastRoll: Boolean = (curTurn == (10,3) || (curTurn == (10,2) && doubleNextTwo == 0 && pins < remainPins))

      val nextScore = curTurn match {
        case (10, 3) => curScore + pins * (doubleNextTwo+doubleNextOne)
        case (10, 2) => curScore + pins * (if (doubleNextTwo==1) (doubleNextTwo+doubleNextOne) else (1+doubleNextTwo+doubleNextOne))
        case _ => curScore + pins * (1+doubleNextTwo+doubleNextOne)
      }

      if (isLastRoll) FinishedGame(nextScore)
      else {
        val isStrike = (curTurn._2 == 1 && pins == 10)
        val isSpare = (curTurn._2 == 2 && remainPins == pins && !(curTurn == (10, 2) && doubleNextTwo == 1))

        val nextTurn: (Int, Int) = curTurn match {
          case (10, i) => (10, i+1)
          case (i, 2) => (i+1, 1)
          case (i, 1) => if (isStrike) (i+1, 1) else (i, 2)
          case _ => (1,1) // Cannot reach here!!
        }

        val nextRemainPins = if (nextTurn._2 == 1 || (nextTurn._1 == 10 && remainPins-pins ==0)) 10 else remainPins-pins

        ProcessingGame(nextTurn, nextScore, nextRemainPins, if (isStrike) 1 else 0, if (isSpare) doubleNextTwo+1 else doubleNextTwo)
      }
    }
  }

  override def score(): Either[Error, Int] = Left(Error("Game is not finished."))
}

case class FinishedGame(totalScore: Int) extends Bowling {
  override def roll(pins: Int): Bowling = ErrorGame(Error("More than 10 frames."))
  override def score(): Either[Error, Int] = Right(totalScore)
}

case class ErrorGame(error: Error) extends Bowling {
  override def roll(pins: Int): Bowling = this
  override def score(): Either[Error, Int] = Left(error)
}

case class Error(errorText: String)