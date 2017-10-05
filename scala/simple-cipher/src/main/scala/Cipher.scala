import scala.util.Random

case class Cipher(keyOpt: Option[String]) {
  val key = keyOpt match {
    case Some(s) => s
    case None => Random.alphanumeric.filter(_.isLower).take(100).mkString
  }

  require(key.forall(_.isLower) && !key.isEmpty)

  def encode(str: String): String =
    str.zip(key)
      .map{ case (c, k) => c.forward(k.index) }.mkString

  def decode(encodedStr: String): String =
    encodedStr.zip(key)
      .map{ case (c, k) => c.backward(k.index) }.mkString

  implicit def charToMyChar(c: Char): MyChar = new MyChar(c)
}

class MyChar(c: Char) {
  val index = c - 'a'

  def forward(length: Int): Char = go(length)
  def backward(length: Int): Char = go(-1 * length)

  private def go(length: Int): Char =
    ('a' + Math.floorMod(index+length, 26)).toChar


}