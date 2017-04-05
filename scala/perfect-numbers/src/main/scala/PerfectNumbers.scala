import NumberType.NumberType

object PerfectNumbers {
  def classify(number: Int): NumberType = {
    val aliquotSum = (1 to number/2).filter(number%_ == 0).sum

    import NumberType._
    if (aliquotSum == number) Perfect
    else if (aliquotSum < number) Deficient
    else Abundant
  }
}

object NumberType extends Enumeration {
  type NumberType = Value

  val Perfect, Deficient, Abundant = Value
}