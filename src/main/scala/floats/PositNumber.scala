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

    // Prepare the raw bit pattern for decoding.
    // For negative posits the standard posit convention is to take the two's-complement
    // of the entire n-bit pattern before decoding the regime/exponent/fraction.
    val maskAll = (BigInt(1) << bits) - 1
    var ui = value & maskAll
    if (sign) {
      // two's complement within n bits
      ui = ((-ui) & maskAll)
    }
    // now drop the sign bit and decode the remaining (bits-1) payload
    val raw = ui & ((BigInt(1) << (bits - 1)) - 1)

    // We'll walk bits from MSB (bits-2) down to LSB using a position index.
    var pos = bits - 2

    // Find regime run (count consecutive identical bits starting at pos)
    val firstBit = ((raw >> pos) & 1).toInt
    var run = 0
    while (pos >= 0 && (((raw >> pos) & 1).toInt == firstBit)) {
      run += 1
      pos -= 1
    }

    // Compute regime k: if ones, k = run - 1; if zeros, k = -run
    regime = if (firstBit == 1) run - 1 else -run

    // If there is a terminating bit (opposite bit) consume it
    if (pos >= 0) {
      pos -= 1
    }

    // Extract exponent bits (up to `es`, limited by remaining bits)
    exponent = 0
    val remainingForExp = math.max(0, pos + 1)
    val expBits = math.min(es, remainingForExp)
    for (i <- 0 until expBits) {
      exponent = (exponent << 1) | (((raw >> pos) & 1).toInt)
      pos -= 1
    }

    // Remaining bits are fraction
    fraction = 0
    val remainingFrac = math.max(0, pos + 1)
    for (i <- 0 until remainingFrac) {
      fraction = (fraction << 1) | (((raw >> pos) & 1).toInt)
      pos -= 1
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