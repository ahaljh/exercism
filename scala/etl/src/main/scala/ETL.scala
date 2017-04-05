/**
  * Created by ahaljh on 2016. 12. 16..
  */
object ETL {
  def transform(old: Map[Int, Seq[String]]) = {
    for {
      (key, seq) <- old
      s <- seq
    } yield (s.toLowerCase, key)
  }
}
