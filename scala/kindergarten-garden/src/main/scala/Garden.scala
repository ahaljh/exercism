/**
  * Created by ahaljh on 2017. 3. 19..
  */
object Garden {
  private val defaultChildren = List("Alice", "Bob", "Charlie", "David",
  "Eve", "Fred", "Ginny", "Harriet",
  "Ileana", "Joseph", "Kincaid", "Larry")

  def defaultGarden(plants: String): Garden = new Garden(defaultChildren, plants)

  def apply(chilren: List[String], plants: String) = new Garden(chilren.sorted, plants)
}

class Garden(children: List[String], plants: String) {
  private val lineArr = plants.split("\n").map(_.grouped(2).toList)
  private val childPlantMap = children.zip {
    lineArr(0).zip(lineArr(1)).map{ case (s1, s2) => s1+s2 }
  }.toMap

  def getPlants(child: String) =
    childPlantMap.get(child).getOrElse("").map(c => Plant.plantNameMap(c))
}

object Plant extends Enumeration {
  type Plant = Value
  val Clover, Grass, Radishes, Violets = Value

  val plantNameMap = Map('C' -> Clover, 'G' -> Grass, 'R' -> Radishes, 'V' -> Violets)
}