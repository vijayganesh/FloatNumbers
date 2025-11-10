package floats
/* Needd to complete as per the logic */

class PositNumber(val bits: Int, val es: Int) {
  private var value: BigInt = 0
  private var sign: Boolean = false
  private var regime: Int = 0
  private var exponent: Int = 0
  private var fraction: BigInt = 0

  // Constructor that takes a binary value
  def this(bits: Int, es: Int, binary: BigInt) = {
    this(bits, es)
    this.value = binary
    decode()
  }

  // Decode a posit number into its components
  private def decode(): Unit = {
    if (value == 0) {
      // Handle zero case
      sign = false
      regime = 0
      exponent = 0
      fraction = 0
      return
    }

    // Extract sign bit
    sign = (value >> (bits - 1)) == 1

    // Get the value without sign bit for processing
    var temp = value & ((BigInt(1) << (bits - 1)) - 1)

    // Find regime
    var regimeBits = 0
    var firstBit = (temp >> (bits - 2)) & 1
    temp = temp << 1
    while (regimeBits < (bits - 2) && ((temp >> (bits - 1)) & 1) == firstBit) {
      regimeBits += 1
      temp = temp << 1
    }
    
    regime = if (firstBit == 1) regimeBits else -regimeBits - 1

    // Extract exponent bits
    exponent = 0
    val maxExponentBits = math.min(es, bits - 2 - regimeBits)
    for (i <- 0 until maxExponentBits) {
      exponent = (exponent << 1) | ((temp >> (bits - 1)) & 1).toInt
      temp = temp << 1
    }

    // Remaining bits are fraction
    fraction = 0
    val remainingBits = bits - 2 - regimeBits - maxExponentBits
    if (remainingBits > 0) {
      for (i <- 0 until remainingBits) {
        fraction = (fraction << 1) | ((temp >> (bits - 1)) & 1).toInt
        temp = temp << 1
      }
    }
  }

  // Construct a posit number from components
  def construct(sign: Boolean, regime: Int, exponent: Int, fraction: BigInt): BigInt = {
    var result = if (sign) BigInt(1) << (bits - 1) else BigInt(0)
    
    // Calculate regime bits
    val regimeBit = if (regime >= 0) 1 else 0
    val regimeBits = math.abs(regime) + 1
    var currentPos = bits - 2
    
    // Add regime bits
    for (i <- 0 until regimeBits) {
      if (regimeBit == 1) {
        result |= (BigInt(1) << currentPos)
      }
      currentPos -= 1
    }
    
    // Add terminating bit
    if (currentPos >= 0) {
      result |= (BigInt(1 - regimeBit) << currentPos)
      currentPos -= 1
    }
    
    // Add exponent bits
    val usableEs = math.min(es, currentPos + 1)
    for (i <- 0 until usableEs) {
      if (((exponent >> (es - 1 - i)) & 1) == 1) {
        result |= (BigInt(1) << currentPos)
      }
      currentPos -= 1
    }
    
    // Add fraction bits
    if (currentPos >= 0) {
      val shiftAmount = currentPos + 1
      val fractionMask = (BigInt(1) << shiftAmount) - 1
      result |= (fraction & fractionMask)
    }
    
    result

    result
  }

  // Getter methods
  def getSign: Boolean = sign
  def getRegime: Int = regime
  def getExponent: Int = exponent
  def getFraction: BigInt = fraction
  def getValue: BigInt = value

  override def toString: String = {
    s"Posit($bits, $es) = {sign: $sign, regime: $regime, exponent: $exponent, fraction: $fraction}"
  }
}