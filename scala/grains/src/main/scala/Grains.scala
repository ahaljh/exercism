/**
  * Created by ahaljh on 2016. 12. 28..
  */
object Grains {
  def square(i: Int, acc: BigInt = 1): Option[BigInt] = {
    if (i < 0 || i > 64) None
    else if (i<=1) Some(acc)
    else square(i-1, acc*2)
  }

  def total = (1 to 64).map(square(_).get).sum

}
