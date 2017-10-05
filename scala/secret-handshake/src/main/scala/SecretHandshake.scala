/**
  * Created by ahaljh on 2017. 5. 3..
  */
object SecretHandshake {

  def handshake(decimalNumber: Int): List[String] = handshake(Integer.toString(decimalNumber, 2))

  def handshake(binaryString: String): List[String] = {
    val convlist = List("wink", "double blink", "close your eyes", "jump")

    if (binaryString.exists(c => c != '0' && c != '1')) Nil
    else {
      val result = binaryString.reverse.zip(convlist)
        .filter{ case (c, s) => c == '1' }
        .map{ case (c, s) => s }

      if (binaryString.length >= 5) result.reverse.toList else result.toList
    }
  }

}
