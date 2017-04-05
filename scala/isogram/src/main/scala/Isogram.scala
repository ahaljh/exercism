object Isogram {
  def isIsogram(str: String): Boolean =
    str.filter(_.isLetter).toLowerCase
      .groupBy(identity).mapValues(_.size)
      .forall(_._2 <= 1)


}