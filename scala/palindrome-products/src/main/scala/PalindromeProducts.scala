case class PalindromeProducts(begin: Int, end: Int) {

  val palindromes = for {
    a <- begin to end
    b <- a to end
    if (isPalindrome(a*b))
  } yield (a*b, a, b)

  def largest: (Int, Set[(Int,Int)]) = {
    val largestPalindrome: Int = palindromes.map(_._1).max
    val numbers: Set[(Int,Int)] = palindromes.filter(_._1 == largestPalindrome)
      .map{ case (c, a, b) => (a, b)}
      .toSet

    (largestPalindrome, numbers)
  }

  def smallest: (Int, Set[(Int,Int)]) = {
    val smallestPalindrome: Int = palindromes.map(_._1).min
    val numbers: Set[(Int,Int)] = palindromes.filter(_._1 == smallestPalindrome)
      .map{ case (c, a, b) => (a, b)}
      .toSet

    (smallestPalindrome, numbers)
  }


  private def isPalindrome(number: Int): Boolean =
    (number.toString.reverse == number.toString)

}
