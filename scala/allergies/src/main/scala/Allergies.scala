import Allergen.Allergen

object Allergies {
  def isAllergicTo(allergen: Allergen, score: Int): Boolean = {
    allergies(score).contains(allergen)
  }

  def allergies(score: Int): List[Allergen] = {
    import Allergen._
    List(Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats)
      .zip(Integer.toBinaryString(score).reverse)
      .filter{ case (allergen, scoreChar) => scoreChar != '0'}
      .map(_._1)
  }

}

object Allergen extends Enumeration {
  type Allergen = Value
  val Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats = Value
}