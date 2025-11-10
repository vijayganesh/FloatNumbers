package org.vricsa.Generators


class con_float_Uint (val expBits:Int, val mantissaBits:Int){
  def _init_(): Unit = {

  }

  def doubleToIEEE(value: Double): Long = {
    // Convert Double to raw long bits
    val longBits = java.lang.Double.doubleToRawLongBits(value)

    // Extract the sign, exponent, and mantissa from the 64-bit double representation
    val sign = (longBits >> 63) & 0x1
    val exponent = (longBits >> 52) & 0x7FF
    val mantissa = longBits & 0xFFFFFFFFFFFFFL

    // Adjust exponent for the new format
    val newExponentBias = (1 << (expBits - 1)) - 1
    val doubleExponentBias = 1023 // Double's exponent bias
    val newExponent = Math.max(0, Math.min((exponent - doubleExponentBias) + newExponentBias, (1 << expBits) - 1))

    // Truncate or round mantissa to fit the specified mantissa bits
    val newMantissa = mantissa >> (52 - mantissaBits)

    // Assemble the IEEE format (sign | exponent | mantissa)
    (sign << (expBits + mantissaBits)) | (newExponent.toLong << mantissaBits) | newMantissa
  }

  // Method to convert IEEE Floating Point Representation back to Double
  def ieeeToDouble(bits: Long): Double = {
    // Extract the sign, exponent, and mantissa from the IEEE format
    val sign = (bits >> (expBits + mantissaBits)) & 0x1
    val exponent = (bits >> mantissaBits) & ((1 << expBits) - 1)
    val mantissa = bits & ((1L << mantissaBits) - 1)
    //println(f" Inside IEEE double Got value as ${bits} \n the sign = ${sign} exp = ${exponent} and mant= ${mantissa} ")
    // Adjust exponent for Double format
    val newExponentBias = (1 << (expBits - 1)) - 1
    val doubleExponentBias = 1023
    val newExponent = Math.max(0, Math.min((exponent - newExponentBias) + doubleExponentBias, 0x7FF))

    // Reconstruct the mantissa for Double format
    val newMantissa = mantissa << (52 - mantissaBits)

    // Assemble the Double format (sign | exponent | mantissa)
    val doubleBits = (sign << 63) | (newExponent.toLong << 52) | newMantissa

    // Convert long bits back to double
    java.lang.Double.longBitsToDouble(doubleBits)
  }

  def doubleToSinglePrecision(value: Double): Int = {
    val floatValue = value.toFloat
    java.lang.Float.floatToIntBits(floatValue)
  }

  def doubleToHalfPrecision(value: Double): Int = {
    val floatValue = value.toFloat
    val intBits = java.lang.Float.floatToIntBits(floatValue)

    // Extract sign, exponent, and mantissa bits
    val sign = (intBits >> 31) & 0x1
    val exponent = (intBits >> 23) & 0xFF
    val mantissa = intBits & 0x7FFFFF

    // Convert to half precision (1 sign, 5 exponent, 10 mantissa)
    val halfExponent = Math.max(0, Math.min(31, exponent - 112)) // Adjust the exponent bias (127 for float, 15 for half)
    val halfMantissa = mantissa >> 13 // Truncate mantissa to fit 10 bits

    // Assemble the half-precision bits
    (sign << 15) | (halfExponent << 10) | halfMantissa
  }


  def doubleToBFloat16(value: Double): Int = {
    val floatValue = value.toFloat
    val intBits = java.lang.Float.floatToIntBits(floatValue)

    // Extract sign, exponent, and mantissa bits
    val sign = (intBits >> 31) & 0x1
    val exponent = (intBits >> 23) & 0xFF
    val mantissa = intBits & 0x7FFFFF

    // Bfloat16: 1 sign, 8 exponent, 7 mantissa (truncated)
    val bfloatMantissa = mantissa >> 16 // Truncate mantissa to fit 7 bits

    // Assemble the bfloat16 bits
    (sign << 15) | (exponent << 7) | bfloatMantissa
  }

  def bfloat16ToDouble(bits: Int): Double = {
    // Extract sign, exponent, and mantissa
    val sign = (bits >> 15) & 0x1
    val exponent = (bits >> 7) & 0xFF
    val mantissa = bits & 0x7F

    // Reconstruct 32-bit float from bfloat16
    val floatBits = (sign << 31) | (exponent << 23) | (mantissa << 16)
    val floatValue = java.lang.Float.intBitsToFloat(floatBits)

    // Convert to double
    floatValue.toDouble
  }
  def halfPrecisionToDouble(bits: Int): Double = {
    // Extract sign, exponent, and mantissa
    val sign = (bits >> 15) & 0x1
    val exponent = (bits >> 10) & 0x1F
    val mantissa = bits & 0x3FF

    // Reconstruct 32-bit float from half-precision
    var floatExponent = 0
    var floatMantissa = 0

    if (exponent == 0) {
      // Subnormal number
      floatExponent = 0
      floatMantissa = mantissa << 13
    } else if (exponent == 31) {
      // Special values (infinity or NaN)
      floatExponent = 255
      floatMantissa = if (mantissa == 0) 0 else mantissa << 13 // NaN if mantissa is not 0
    } else {
      // Normalized number
      floatExponent = exponent + 112 // Adjust bias (127 - 15 = 112)
      floatMantissa = mantissa << 13
    }

    // Combine the sign, exponent, and mantissa into 32-bit float bits
    val floatBits = (sign << 31) | (floatExponent << 23) | floatMantissa
    val floatValue = java.lang.Float.intBitsToFloat(floatBits)

    // Convert to double
    floatValue.toDouble
  }

  def singlePrecisionToDouble(bits: Int): Double = {
    // Convert 32-bit integer bits directly to float
    val floatValue = java.lang.Float.intBitsToFloat(bits)

    // Convert to double
    floatValue.toDouble
  }


}
