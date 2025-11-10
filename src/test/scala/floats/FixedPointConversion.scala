package floats

object FixedPointConversion {
  /**
   * Creates a conversion helper with fixed number of fractional bits
   * @param fractionalBits The number of fractional bits to use for all conversions
   * @return A conversion helper with fixed fractional bits
   */
  def apply(fractionalBits: Int) = new FixedPointConverter(fractionalBits)

  /**
   * Convert a double to fixed-point representation
   * @param d The double value to convert
   * @param fractionalBits The number of fractional bits in the fixed-point format
   * @return BigInt representing the fixed-point number
   */
  def toFixed(d: Double, fractionalBits: Int): BigInt = {
    val scale = Math.pow(2, fractionalBits)
    BigInt((d * scale).toLong)
  }
  
  /**
   * Convert a fixed-point number back to double
   * @param x The fixed-point number as BigInt
   * @param fractionalBits The number of fractional bits in the fixed-point format
   * @return Double representation of the fixed-point number
   */
  def fromFixed(x: BigInt, fractionalBits: Int): Double = {
    val scale = Math.pow(2, fractionalBits)
    x.toDouble / scale
  }

  /**
   * Helper class for fixed-point conversions with predetermined number of fractional bits
   */
  class FixedPointConverter(fractionalBits: Int) {
    def toFixed(d: Double): BigInt = FixedPointConversion.toFixed(d, fractionalBits)
    def fromFixed(x: BigInt): Double = FixedPointConversion.fromFixed(x, fractionalBits)
  }

  /**
   * Calculate the maximum representable value for a given fixed-point format
   * @param integerBits The number of integer bits (including sign bit)
   * @param fractionalBits The number of fractional bits
   * @return Maximum representable value as Double
   */
  def maxValue(integerBits: Int, fractionalBits: Int): Double = {
    Math.pow(2, integerBits - 1) - Math.pow(2, -fractionalBits)
  }

  /**
   * Calculate the minimum representable positive value for a given fixed-point format
   * @param fractionalBits The number of fractional bits
   * @return Minimum representable positive value as Double
   */
  def minPositiveValue(fractionalBits: Int): Double = {
    Math.pow(2, -fractionalBits)
  }
}