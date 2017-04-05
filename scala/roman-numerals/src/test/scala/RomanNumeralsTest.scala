import org.scalatest._

class RomanNumeralsSpecs extends FunSpec with Matchers {

  it ("0 equals empty string") {
    RomanNumeral.toNumerals(0) should be ("")
  }

  it ("1 equals I") {
    RomanNumeral.toNumerals(1) should be ("I")
  }

  it ("2 equals II") {
    RomanNumeral.toNumerals(2) should be ("II")
  }

  it ("3 equals III") {
    RomanNumeral.toNumerals(3) should be ("III")
  }

  it ("4 equals IV") {
    RomanNumeral.toNumerals(4) should be ("IV")
  }

  it ("5 equals V") {
    RomanNumeral.toNumerals(5) should be ("V")
  }

  it ("6 equals VI") {
    RomanNumeral.toNumerals(6) should be ("VI")
  }

  it ("9 equals IX") {
    RomanNumeral.toNumerals(9) should be ("IX")
  }

  it ("27 equals XXVII") {
    RomanNumeral.toNumerals(27) should be ("XXVII")
  }

  it ("48 equals XLVIII") {
    RomanNumeral.toNumerals(48) should be ("XLVIII")
  }

  it ("59 equals LIX") {
    RomanNumeral.toNumerals(59) should be ("LIX")
  }

  it ("93 equals XCIII") {
    RomanNumeral.toNumerals(93) should be ("XCIII")
  }

  it ("141 equals CXLI") {
    RomanNumeral.toNumerals(141) should be ("CXLI")
  }

  it ("402 equals CDII") {
    RomanNumeral.toNumerals(402) should be ("CDII")
  }

  it ("575 equals DLXXV") {
    RomanNumeral.toNumerals(575) should be ("DLXXV")
  }

  it ("911 equals CMXI") {
    RomanNumeral.toNumerals(911) should be ("CMXI")
  }

  it ("1024 equals MXXIV") {
    RomanNumeral.toNumerals(1024) should be ("MXXIV")
  }

  it ("3000 equals MMM") {
    RomanNumeral.toNumerals(3000) should be ("MMM")
  }
}
