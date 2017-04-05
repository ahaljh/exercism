import org.scalatest.{FunSuite, Matchers}

class LuhnTest extends FunSuite with Matchers {
  test("single digit strings can not be valid") {
    Luhn.validate("1") should be (false)
  }

  test("A single zero is invalid") {
    Luhn.validate("0") should be (false)
  }

  test("valid Canadian SIN") {
    Luhn.validate("046 454 286") should be (true)
  }

  test("invalid Canadian SIN") {
    Luhn.validate("046 454 287") should be (false)
  }

  test("invalid credit card") {
    Luhn.validate("8273 1232 7352 0569") should be (false)
  }

  test("strings that contain non-digits are not valid") {
    Luhn.validate("827a 1232 7352 0569") should be (false)
  }
}

