package floats

import chisel3._
import chisel3.util._

// Fixed Point Number class with configurable integer and fractional bits
class FixedPoint(val integerBits: Int, val fractionalBits: Int) extends Bundle {
  require(integerBits > 0, "Integer bits must be positive")
  require(fractionalBits > 0, "Fractional bits must be positive")
  
  val totalBits = integerBits + fractionalBits
  // Sign bit is included in integer bits
  val value = SInt(totalBits.W)
  
  // Helper function to create a fixed point from raw bits
  def fromBits(rawBits: SInt): FixedPoint = {
    val fp = Wire(new FixedPoint(integerBits, fractionalBits))
    fp.value := rawBits
    fp
  }
}

// Fixed Point Arithmetic Operations
class FixedPointArithmetic(integerBits: Int, fractionalBits: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(new FixedPoint(integerBits, fractionalBits))
    val b = Input(new FixedPoint(integerBits, fractionalBits))
    val operation = Input(UInt(2.W))  // 00: Add, 01: Subtract, 10: Multiply
    val result = Output(new FixedPoint(integerBits, fractionalBits))
    val overflow = Output(Bool())
  })

  // Operation Constants
  val ADD = 0.U(2.W)
  val SUB = 1.U(2.W)
  val MUL = 2.U(2.W)

  // Intermediate wider width for calculations to handle overflow
  val widenedBits = (integerBits + fractionalBits + 1).W
  val doubleWidenedBits = (2 * (integerBits + fractionalBits)).W

  // Addition Logic
  def doAdd(a: SInt, b: SInt): (SInt, Bool) = {
    val sum = Wire(SInt(widenedBits))
    sum := a +& b  // +& operator includes carry bit
    val overflow = sum(sum.getWidth-1) =/= sum(sum.getWidth-2)
    (sum(sum.getWidth-2, 0).asSInt, overflow)
  }

  // Multiplication Logic
  def doMultiply(a: SInt, b: SInt): (SInt, Bool) = {
    val product = Wire(SInt(doubleWidenedBits))
    product := (a * b) >> fractionalBits.U
    
    // Check overflow
    val expectedSign = a(a.getWidth-1) ^ b(b.getWidth-1)
    val actualSign = product(product.getWidth-1)
    val overflow = expectedSign =/= actualSign
    
    // Extract the relevant bits
    val resultBits = product(integerBits + fractionalBits - 1, 0)
    (resultBits.asSInt, overflow)
  }

  // Main Operation Logic
  val resultValue = Wire(SInt(io.a.value.getWidth.W))
  val overflowFlag = Wire(Bool())
  
  when(io.operation === ADD) {
    val (res, ovf) = doAdd(io.a.value, io.b.value)
    resultValue := res
    overflowFlag := ovf
  }.elsewhen(io.operation === SUB) {
    val (res, ovf) = doAdd(io.a.value, -io.b.value)
    resultValue := res
    overflowFlag := ovf
  }.elsewhen(io.operation === MUL) {
    val (res, ovf) = doMultiply(io.a.value, io.b.value)
    resultValue := res
    overflowFlag := ovf
  }.otherwise {
    resultValue := 0.S
    overflowFlag := false.B
  }

  io.result.value := resultValue
  io.overflow := overflowFlag
}

// Fixed Point Constants and Utilities Object
object FixedPointUtils {
  def fromInt(value: Int, intBits: Int, fracBits: Int): SInt = {
    val scaled = value << fracBits
    val width = (intBits + fracBits).W
    scaled.asSInt(width)
  }

  def fromDouble(value: Double, intBits: Int, fracBits: Int): SInt = {
    val scaled = (value * (1 << fracBits)).toInt
    val width = (intBits + fracBits).W
    scaled.asSInt(width)
  }
}