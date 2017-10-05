case class Stack(bookMap: Map[Int,Int], excepts: Set[Set[Int]], acc: Double)

object BookStore {
  val bookGroup = Set(1,2,3,4,5).subsets().toList.filter(_.size > 1).sortWith(_.size >_.size)  // List[Set[Int]]
  val costMap: Map[Int, Double] = Map(5 -> 5*8*0.75, 4 -> 4*8*0.8, 3 -> 3*8*0.9, 2 -> 2*8*0.95, 1 -> 8, 0 -> 0)

  def total(books: List[Int]): Double = {

    @annotation.tailrec
    def process(item: Stack, curMinValue: Double, stack: List[Stack]): Double = {
      //println(s"bookMap:$bookMap|excepts:$excepts|acc:$acc|curMinValue:$curMinValue|stack:$stack")
      val (bookMap, excepts, acc) = (item.bookMap, item.excepts, item.acc)

      if (bookMap.values.forall(_==1)) {
        stack match {
          case Nil => min(curMinValue, acc+costMap(bookMap.size))
          case h::t => process(h, min(curMinValue, acc+costMap(bookMap.size)), t)
        }
      }
      else if (bookMap.size == 1) {
        stack match {
          case Nil => min(curMinValue, acc + 8*bookMap.values.sum)
          case h::t => process(h, min(curMinValue, acc + 8*bookMap.values.sum), t)
        }
      }
      else {
        val group = bookGroup.filter(set => set.size <= bookMap.keySet.size && !excepts.contains(set))

        group.find(_.subsetOf(bookMap.keySet)) match {
          case None => {
            stack match {
              case Nil => min(curMinValue, acc + 8*bookMap.values.sum)
              case h::t => process(h, min(curMinValue, acc + 8*bookMap.values.sum), t)
            }
          }
          case Some(set) => {
            process(Stack(bookMap-set, excepts, acc + costMap(set.size)), curMinValue, Stack(bookMap, excepts+set, acc) :: stack)
          }
        }
      }
    }

    process(Stack(books.groupBy(identity).mapValues(_.size), Set(), 0), books.size * 8, Nil)

  }

  def min(a: Double, b: Double): Double = if (a > b) b else a

  implicit def toMyMap(m: Map[Int, Int]): MyMap = new MyMap(m)
}

class MyMap(m: Map[Int, Int]) {
  def - (set: Set[Int]): Map[Int, Int] = {
    m
      .map{ case (k,v) => if (set.contains(k)) (k, v-1) else (k, v) }
      .filter{ case (k,v) => v > 0 }
  }
}
