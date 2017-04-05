import scala.annotation.tailrec

object BinarySearch {
  def search(ints: Array[Int], number: Int): Option[Int] = {
    @tailrec
    def process(startIdx: Int, endIdx: Int): Option[Int] = {
      if (startIdx > endIdx) None
      else {
        val midIdx = startIdx + (endIdx-startIdx)/2

        if (ints(midIdx) == number) Some(midIdx)
        else if (ints(midIdx) < number) process(midIdx+1, endIdx)
        else process(startIdx, midIdx-1)  // if (ints(midIdx) > number)
      }
    }

    if (ints.isEmpty) None
    else process(0, ints.length-1)
  }

}