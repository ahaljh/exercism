object Prime {
  def nth(index: Int): Option[Int] = {
    lazy val primes: Stream[Int] = 2 #:: Stream.from(3).filter(isPrime(_))

    def isPrime(n: Int): Boolean = {
      val rootN: Int = math.sqrt(n).toInt

      (2 to rootN).forall(n % _ != 0)
    }

    if (index <= 0) None
    else Some(primes(index-1))
  }

}
