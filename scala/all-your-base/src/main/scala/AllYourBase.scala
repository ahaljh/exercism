/**
  * Created by ahaljh on 2017. 3. 13..
  */
object AllYourBase {
  def rebase(fromBase: Int, ints: List[Int], toBase: Int): Option[List[Int]] = {
    def convertToNBase(value: Int, base: Int): List[Int] = {
      @annotation.tailrec
      def process(v: Int, base: Int, acc: List[Int]): List[Int] =
        if (v <= 0) acc
        else process(v/base, base, v%base :: acc)

      process(value, base, Nil)
    }


    if (fromBase <= 1 || toBase <= 1 || ints.exists(_ < 0) || ints.exists(_ >= fromBase)) None
    else {
      val value = ints.reverse.zipWithIndex
        .map{ case (number: Int, index: Int) => number * math.pow(fromBase, index).toInt }.sum

      Some(convertToNBase(value, toBase))
    }
  }

}
