package org.vricsa.Generators

import svsim.CommonCompilationSettings.Timescale.Unit.s


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

  // Convert a Double to a posit bit-pattern (returned as BigInt)
  // n: total bits of posit, es: exponent size
  def doubleToPosit(value: Double, n: Int, es: Int): BigInt = {
    if (value == 0.0) return BigInt(0)

    val sign = if (value < 0) 1 else 0
    val absV = math.abs(value)

    // useed = 2^(2^es)
    val useed = math.pow(2.0, math.pow(2.0, es).toDouble)

    // Determine regime k
    val k = if (absV == 0.0) 0 else math.floor(math.log(absV) / math.log(useed)).toInt
    val regimeBits = math.abs(k) + 1
 println(s" dtop \t regimeBits=$regimeBits k = $k ")
    var result = if (sign == 1) BigInt(1) << (n - 1) else BigInt(0)
    var currentPos = n - 2 // next bit position after sign (0-based)

    // Write regime bits
    if (k >= 0) {
      // k+1 ones
      for (_ <- 0 until regimeBits if currentPos >= 0) {
        result |= BigInt(1) << currentPos
        currentPos -= 1
      }
      // terminating zero (if space) left as 0
      if (currentPos >= 0) currentPos -= 1
    } else {
      // |k|+1 zeros then terminating one
      for (_ <- 0 until regimeBits if currentPos >= 0) {
        // zeros -> do nothing
        currentPos -= 1
      }
      if (currentPos >= 0) {
        result |= BigInt(1) << currentPos
        currentPos -= 1
      }
    }

    // Add exponent bits (as many as fit, up to es)
    val usableEs = math.max(0, math.min(es, currentPos + 1))
    var exponent = 0
    if (usableEs > 0) {
      // compute exponent by removing regime contribution
      val rem = absV / math.pow(useed, k)
      // e = floor(log2(rem)) but clamp into range
      val rawE = if (rem <= 0.0) 0 else math.floor(math.log(rem) / math.log(2.0)).toInt
      exponent = math.max(0, math.min((1 << es) - 1, rawE))
      // write exponent MSB-first
      for (i <- 0 until usableEs if currentPos >= 0) {
        val bit = (exponent >> (usableEs - 1 - i)) & 1
        if (bit == 1) result |= BigInt(1) << currentPos
        currentPos -= 1
      }
    }

    // Fraction bits fill the rest
    val fracBits = currentPos + 1
    if (fracBits > 0) {
      val rem2 = absV / (math.pow(useed, k) * math.pow(2.0, exponent))
      var frac = rem2 / 1.0 - 1.0
      if (frac.isNaN || frac.isInfinite) frac = 0.0
      var fracVal = BigInt(0)
      for (i <- 0 until fracBits) {
        frac *= 2.0
        if (frac >= 1.0) {
          fracVal |= BigInt(1) << (fracBits - 1 - i)
          frac -= 1.0
        }
      }
      val mask = (BigInt(1) << fracBits) - 1
      result |= (fracVal & mask)
    }

    result
  }

  // Convert a posit bit-pattern (BigInt) to Double
  def positToDouble(bits: BigInt, n: Int, es: Int): Double = {
    if (bits == BigInt(0)) return 0.0

    val p = new floats.PositNumber(n, es, bits)
    val signMultiplier = if (p.getSign) -1.0 else 1.0
    val k = p.getRegime //if (p.getRegime > 0) p.getRegime -1  else p.getRegime +1 
    val exponent = p.getExponent

    println(s" \t sign=$signMultiplier, regime=$k, exponent=$exponent")

    // Determine how many exponent bits were actually used during decoding
    val regimeBits = math.abs(k) + 1
    val usedEs = math.max(0, math.min(es, n - 1 - regimeBits))
    val fracBits = math.max(0, n - 1 - regimeBits - usedEs)
    println(s" \t regime = $regimeBits usedEs=$usedEs, fracBits=$fracBits")
    val fracInt = p.getFraction
    val fracVal = if (fracBits > 0) {
      fracInt.toDouble / (BigInt(1) << fracBits).toDouble
    } else 0.0

    println(s" \t fracVal=$fracVal")

    val mantissa = 1.0 + fracVal
    val useed = math.pow(2.0, math.pow(2.0, es).toDouble)
    println(s" \t mantissa=$mantissa, useed=$useed k =$k")
    signMultiplier * math.pow(useed, k) * math.pow(2.0, exponent) * mantissa
  }


}
