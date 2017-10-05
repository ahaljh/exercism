sealed trait BeerSong {
  def bottles: String
  def second: String

  def first: String = s"${bottles.capitalize} of beer on the wall, $bottles of beer."
  def verse: String = first + "\n" +second + "\n"
}

case object EmptyBeer extends BeerSong {
  override def bottles: String = "no more bottles"
  override def second: String =
    "Go to the store and buy some more, 99 bottles of beer on the wall."
}

case object JustOneBeer extends BeerSong {
  override def bottles: String = "1 bottle"
  override def second: String =
    "Take it down and pass it around, no more bottles of beer on the wall."
}

case class MoreThanOneBeer(bottle: Int) extends BeerSong {
  val s = if (bottle > 2) "s" else ""

  override def bottles: String = s"$bottle bottles"
  override def second: String =
    s"Take one down and pass it around, ${bottle-1} bottle${s} of beer on the wall."
}


object BeerSong {
  def verse(no: Int): String = {
    val beer: BeerSong = no match {
      case 0 => EmptyBeer
      case 1 => JustOneBeer
      case n@_ => MoreThanOneBeer(n)
    }

    beer.verse
  }

  def verses(start: Int, end: Int): String = {
    (start to end by -1).map(verse _).mkString("\n")
  }

}
