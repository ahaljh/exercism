/**
  * Created by ahaljh on 2017. 3. 31..
  */

sealed trait Bst {
  def value: Int
  def insert(leafValue: Int): Bst
  def left: Option[Bst]
  def right: Option[Bst]
}

case object Empty extends Bst {
  override def value = throw new Exception("Empty")
  override def insert(newValue: Int): Bst = Node(newValue, Empty, Empty)
  override def left: Option[Bst] = None
  override def right: Option[Bst] = None
}

case class Node(value: Int, leftNode: Bst, rightNode: Bst) extends Bst {
  override def insert(newValue: Int): Bst = {
    if (newValue <= value) Node(value, leftNode.insert(newValue), rightNode)
    else Node(value, leftNode, rightNode.insert(newValue))
  }

  override def left: Option[Bst] = leftNode match {
    case Empty => None
    case node => Some(node)
  }
  override def right: Option[Bst] = rightNode match {
    case Empty => None
    case node => Some(node)
  }
}

object Bst {
  def apply(value: Int): Bst = Node(value, Empty, Empty)

  def fromList(ints: List[Int]): Bst = ints match {
    case Nil => Empty
    case h::t => {
      t.foldLeft(Node(h, Empty, Empty): Bst) { (bst, v) => bst.insert(v) }
    }
  }

  def toList(bst: Bst): List[Int] = bst match {
    case Empty => Nil
    case Node(value, leftNode, rightNode) => toList(leftNode) ++ List(value) ++ toList(rightNode)
  }
}
