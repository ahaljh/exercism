sealed trait CustomSet
case object EmptySet extends CustomSet
case class Node(value: Int, left: CustomSet, right: CustomSet) extends CustomSet

object CustomSet {

  def fromList(list: List[Int]): CustomSet = list match {
    case Nil => EmptySet
    case h::t => Node(h, fromList(list.filter(_ < h)), fromList(list.filter(_ > h)))
  }

  def empty(set: CustomSet): Boolean = set match {
    case EmptySet => true
    case _ => false
  }

  def member(set: CustomSet, no: Int): Boolean = set match {
    case EmptySet => false
    case Node(v, _, _) if v == no => true
    case Node(v, l, r) => if (no > v) member(r, no) else member(l, no)
  }

  def isSubsetOf(subset: CustomSet, superset: CustomSet): Boolean = subset match {
    case EmptySet => true
    case Node(v, l, r) => member(superset, v) && isSubsetOf(l, superset) && isSubsetOf(r, superset)
  }

  def isDisjointFrom(set1: CustomSet, set2: CustomSet): Boolean = set1 match {
    case EmptySet => true
    case Node(v, l, r) => !member(set2, v) && isDisjointFrom(l, set2) && isDisjointFrom(r, set2)
  }

  def isEqual(set1: CustomSet, set2: CustomSet): Boolean = {
    isSubsetOf(set1, set2) && isSubsetOf(set2, set1)
  }

  def insert(set: CustomSet, i: Int): CustomSet = set match {
    case EmptySet => Node(i, EmptySet, EmptySet)
    case Node(v, l, r) if i == v => set // Node(v, l, r)
    case Node(v, l, r) => if (i > v) Node(v, l, insert(r, i)) else Node(v, insert(l, i), r)
  }

  def intersection(set1: CustomSet, set2: CustomSet): CustomSet = {
    def process(s1: CustomSet, s2: CustomSet, acc: CustomSet): CustomSet = s1 match {
      case EmptySet => acc
      case Node(v, l, r) => process(union(l, r), s2, if (member(s2, v)) insert(acc, v) else acc)
    }

    process(set1, set2, EmptySet)
  }

  def difference(set1: CustomSet, set2: CustomSet): CustomSet = {
    def process(s1: CustomSet, s2: CustomSet, acc: CustomSet): CustomSet = s1 match {
      case EmptySet => acc
      case Node(v, l, r) => process(union(l, r), s2, if (member(s2, v)) acc else insert(acc, v))
    }

    process(set1, set2, EmptySet)
  }

  def union(set1: CustomSet, set2: CustomSet): CustomSet = {
    val list1 = toList(set1)
    val list2 = toList(set2)

    fromList(list1 ++ list2)
  }

  private def toList(set: CustomSet): List[Int] = set match {
    case EmptySet => Nil
    case Node(v, l, r) => v :: toList(l) ++ toList(r)
  }
}


