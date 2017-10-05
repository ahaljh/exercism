
class Deque[A] {
  private var data: Node[A] = Node(None)

  def push(item: A): Unit = {
    val newNode: Node[A] = Node(Some(item))
    if (data.isEmpty) {
      data = newNode
    } else {
      val lastNode = data.getLastNode
      lastNode.next = Some(newNode)
      newNode.prev = Some(lastNode)
    }
  }

  def pop: Option[A] = {
    val lastNode = data.getLastNode

    lastNode.prev match {
      case None => data = Node(None)
      case Some(node) => node.next = None
    }

    lastNode.value
  }

  def unshift(item: A): Unit = {
    val newNode = Node(Some(item))

    if (!data.isEmpty) {
      data.prev = Some(newNode)
      newNode.next = Some(data)
    }

    data = newNode
  }

  def shift: Option[A] = {
    val firstNode = data

    data.next match {
      case None => data = Node(None)
      case Some(node) => node.prev = None; data = node
    }

    firstNode.value
  }
}

object Deque {
  def apply[A](): Deque[A] = new Deque[A]()
}


case class Node[A](var value: Option[A]) {
  var prev: Option[Node[A]] = None
  var next: Option[Node[A]] = None

  def isEmpty: Boolean = value match {
    case None => true
    case _ => false
  }

  def getLastNode: Node[A] = next.map(_.getLastNode).getOrElse(this)
}




