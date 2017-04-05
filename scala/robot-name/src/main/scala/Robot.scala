import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by ahaljh on 2016. 12. 6..
  */
class Robot() {
  makeNewName()

  private def pickNewName: String = {
    def getRandomString(range: Seq[Char], len: Int): String = {
      Seq.fill(len){
        range(Random.nextInt(range.size))
      }.mkString
    }

    getRandomString(('A' to 'Z'), 2) + getRandomString(('0' to '9'), 3)
  }

  @tailrec
  private def makeNewName() : String = {
    val newName = pickNewName
    if (Robot.usedNames.contains(newName)) makeNewName()
    else {
      Robot.usedNames ::= newName
      newName
    }
  }

  def name: String = Robot.usedNames.head

  def reset(): Unit = {
    makeNewName()
  }

}

object Robot {
  private var usedNames: List[String] = Nil
}