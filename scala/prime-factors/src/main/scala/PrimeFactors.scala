/**
  * Created by ahaljh on 2017. 2. 25..
  */
object PrimeFactors {
  def forNumber(l: Long): List[Long] = {
    @annotation.tailrec
    def process(num: Long, curDivisor: Long, acc: List[Long]): List[Long] = {
      if (num == 1) acc.reverse
      else if (num % curDivisor == 0) process(num/curDivisor, curDivisor, curDivisor::acc)
      else process(num, curDivisor+1, acc)
    }

    process(l, 2, Nil)
  }

}