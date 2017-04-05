/**
  * Created by ahaljh on 2017. 2. 5..
  */
case class Triangle(a: Int, b: Int, c: Int) {
  def triangleType = {
    if (a >= b+c || b >= a+c || c >= a+b) TriangleType.Illogical
    else if (a <= 0 || b <= 0 || c <= 0) TriangleType.Illogical
    else if (a == b && b == c) TriangleType.Equilateral
    else if (a == b || b == c || c == a) TriangleType.Isosceles
    else TriangleType.Scalene
  }
}

object TriangleType extends Enumeration {
  val Equilateral, Isosceles, Scalene, Illogical = Value
}