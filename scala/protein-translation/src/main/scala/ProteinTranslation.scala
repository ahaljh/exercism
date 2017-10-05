
object ProteinTranslation {
  val transMap: Map[String, String] = List(
    (Seq("AUG") , "Methionine"),
    (Seq("UUU", "UUC") , "Phenylalanine"),
    (Seq("UUA", "UUG") , "Leucine"),
    (Seq("UCU", "UCC", "UCA", "UCG"), "Serine"),
    (Seq("UAU", "UAC") , "Tyrosine"),
    (Seq("UGU", "UGC") , "Cysteine"),
    (Seq("UGG") , "Tryptophan"),
    (Seq("UAA", "UAG", "UGA") , "STOP")
  ).flatMap{ case (seq, v) => seq.map((_, v)) }.toMap

  def translate(rna: String): Seq[String] =
    rna.grouped(3).map(transMap.getOrElse(_, "")).takeWhile(_ != "STOP").toSeq


}
