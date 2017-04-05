import scala.collection.immutable.SortedMap

/**
  * Created by ahaljh on 2016. 12. 4..
  */
class School {
  import scala.collection.mutable.{Map => MMap}
  private val gradeDb: MMap[Int, Seq[String]] = MMap()


  def db: Map[Int, Seq[String]] = gradeDb.toMap

  def add(student: String, g: Int): Unit =
    gradeDb.put(g, grade(g) :+ student)

  def grade(g: Int) = gradeDb.getOrElse(g, Seq())

  def sorted: Map[Int, Seq[String]] =
    SortedMap(gradeDb.toSeq: _*).mapValues(_.sortBy(identity))


}
