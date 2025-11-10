package floats.posit

import org.scalatest._
import flatspec._
import matchers._
import floats.PositNumber

class PositNumberSpec extends AnyFlatSpec with should.Matchers {

  "A PositNumber" should "correctly initialize with zero" in {
    val posit = new PositNumber(8, 2)  // 8-bit posit with es=2
    posit.getSign should be (false)
    posit.getRegime should be (0)
    posit.getExponent should be (0)
    posit.getFraction should be (BigInt(0))
  }

  it should "correctly decode a simple positive number" in {
    // Create a positive number: 01000000 (8-bit posit)
    val posit = new PositNumber(8, 2, BigInt("01000000", 2))
    posit.getSign should be (false)  // Positive
    posit.getRegime should be (1)    // One '1' in regime
    posit.getExponent should be (0)  // Exponent bits are 00
    posit.getFraction should be (BigInt(0))
  }

  it should "correctly decode a simple negative number" in {
    // Create a negative number: 11000000 (8-bit posit)
    val posit = new PositNumber(8, 2, BigInt("11000000", 2))
    posit.getSign should be (true)   // Negative
    posit.getRegime should be (1)    // One '1' in regime
    posit.getExponent should be (0)  // Exponent bits are 00
    posit.getFraction should be (BigInt(0))
  }

  it should "correctly handle different regime patterns" in {
    // Test regime pattern 001 (regime = -2)
    val posit1 = new PositNumber(8, 2, BigInt("00100000", 2))
    posit1.getRegime should be (-2)

    // Test regime pattern 110 (regime = 1)
    val posit2 = new PositNumber(8, 2, BigInt("11000000", 2))
    posit2.getRegime should be (1)
  }

  it should "correctly handle exponent bits" in {
    // Test with es=2, exponent bits = 11
    val posit = new PositNumber(8, 2, BigInt("01011000", 2))
    posit.getExponent should be (3)  // Binary 11 = 3
  }

  it should "correctly handle fraction bits" in {
    // Test with fraction bits = 101
    val posit = new PositNumber(8, 2, BigInt("01000101", 2))
    posit.getFraction should be (BigInt("101", 2))
  }

  it should "correctly construct numbers from components" in {
    val posit = new PositNumber(8, 2)
    
    // Construct: positive, regime=1, exponent=2, fraction=1
    val constructed = posit.construct(
      sign = false,
      regime = 1,
      exponent = 2,
      fraction = BigInt(1)
    )
    
    // Create a new posit from the constructed value to verify components
    val decoded = new PositNumber(8, 2, constructed)
    decoded.getSign should be (false)
    decoded.getRegime should be (1)
    decoded.getExponent should be (2)
    decoded.getFraction should be (BigInt(1))
  }

  it should "handle edge cases" in {
    // Test with all bits set to 1
    val maxPosit = new PositNumber(8, 2, BigInt("11111111", 2))
    maxPosit.getSign should be (true)

    // Test with all bits set to 0 except sign
    val minPosit = new PositNumber(8, 2, BigInt("10000000", 2))
    minPosit.getSign should be (true)
  }

  it should "maintain consistency between construction and decoding" in {
    val original = new PositNumber(8, 2)
    val value = original.construct(
      sign = false,
      regime = 1,
      exponent = 2,
      fraction = BigInt("101", 2)
    )
    
    val decoded = new PositNumber(8, 2, value)
    decoded.getSign should be (false)
    decoded.getRegime should be (1)
    decoded.getExponent should be (2)
    decoded.getFraction should be (BigInt("101", 2))
  }

  it should "properly format toString output" in {
    val posit = new PositNumber(8, 2, BigInt("01010101", 2))
    posit.toString should include ("sign")
    posit.toString should include ("regime")
    posit.toString should include ("exponent")
    posit.toString should include ("fraction")
  }
}