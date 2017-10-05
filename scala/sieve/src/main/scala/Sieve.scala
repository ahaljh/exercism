/**
  * Created by ahaljh on 2017. 5. 4..
  */
object Sieve {
  def primesUpTo(limit: Int): List[Int] = {
    def process(primes: List[Int], acc: List[Int]): List[Int] = acc match {
      case h::t => process(h::primes, t.filter(_%h != 0))
      case Nil => primes.reverse
    }

    process(Nil, (2 to limit).toList)
  }

}
