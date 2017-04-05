/**
  * Created by ahaljh on 2016. 11. 22..
  */
object SumOfMultiples {
  def sumOfMultiples(ints: List[Int], to: Int) = {
    (1 until to).filter(i => {
      ints.exists(i % _ == 0)
    }).sum
  }

}
