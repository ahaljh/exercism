case class Domino(a: Int, b: Int, index: Int) {
  def swap: Domino = Domino(b, a, index)
}

object Dominoes {

  def chain(dominoes: List[(Int, Int)]): Option[List[(Int, Int)]] = {
    val allDominos: List[Domino] = dominoes
      .zipWithIndex
      .map{ case ((a, b), index) => Domino(a, b, index) }

    // nextCandidate only use backtracking
    def process(acc: List[Domino], stack: List[(List[Domino], List[Domino])], nextCandidate: Option[List[Domino]]): Option[List[(Int,Int)]] = {
      val remain = allDominos.filterNot( d => acc.map(_.index).contains(d.index) )
      val nexts = nextCandidate match {
        case Some(list) => list
        case None => acc match {
          case Nil => remain
          case h::t => remain
              .filter ( d => (d.a == h.b || d.b == h.b) )
              .map ( d => if (d.a == h.b) d else d.swap )
        }
      }

      if (acc.size == dominoes.size) Some(acc.map(d => (d.a, d.b)).reverse)
      else if (nexts.size == 0) {
        stack match {
          case Nil => None
          case h::t => process(h._1, t, Some(h._2)) // backtracking
        }
      }
      else if (nexts.size == 1) process(nexts.head::acc, stack, None)
      else process(nexts.head::acc, (acc, nexts.tail)::stack, None)

    }

    val allTwice = dominoes.flatMap{ case (a, b) => List(a, b) }
      .groupBy(identity)
      .mapValues(_.size)
      .values
      .forall(_%2 == 0)

    if (!allTwice) None
    else process(Nil, Nil, None)

  }
}
