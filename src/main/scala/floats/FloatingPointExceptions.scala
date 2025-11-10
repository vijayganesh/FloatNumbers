package org.vricsa.floats
// package floats

import chisel3._
import chisel3.util._
object FloatingPointExceptions {
  var expBits: Int = 0
  var mantBits: Int = 0

  def init(exp: Int, mant: Int): Unit = {
    expBits = exp
    mantBits = mant
  }

  // Function to unpack the combined input into sign, exponent, and mantissa
  def unpack(input: UInt): (UInt, UInt, UInt) = {
    val sign = input(expBits + mantBits)
    val exponent = input(expBits + mantBits - 1, mantBits)
    val mantissa = input(mantBits - 1, 0)
   // printf("The Unpack for %d is sign = %d exp = %d manti = %d \n",input,sign,exponent,mantissa)
    (sign, exponent, mantissa)
  }

  // Function to detect if the floating-point number is zero
  def isZero(exponent: UInt, mantissa: UInt): Bool = {
    exponent === 0.U && mantissa === 0.U
  }

  // Function to detect if the floating-point number is Infinity
  def isInf(exponent: UInt, mantissa: UInt): Bool = {
    exponent.andR && mantissa === 0.U
  }

  // Function to detect if the floating-point number is NaN
  def isNaN(exponent: UInt, mantissa: UInt): Bool = {
    exponent.andR && mantissa =/= 0.U
  }

  def generateInfinity(sign: Bool): UInt = {
    val signBit = sign.asUInt
    val exps = ((1 << expBits) - 1).U(expBits.W) // All exponent bits are 1s
    val mants = 0.U(mantBits.W)         // All mantissa bits are 0s
    Cat(signBit, exps, mants)
  }
  def generateNaN(): UInt = {
    val signBit = 0.U(1.W)                  // Sign bit is typically 0 for NaN
    val exps = ((1 << expBits) - 1).U(expBits.W) // All exponent bits are 1s
    val mants = 1.U(mantBits.W)         // Non-zero mantissa bits for NaN
    Cat(signBit, exps, mants)
  }

  // Combine NaN, Infinity, Zero detections for easy use
  def checkExceptions(input: UInt): (Bool, Bool, Bool) = {
    val (_, exponent, mantissa) = unpack(input)
    val zero = isZero(exponent, mantissa)
    val inf = isInf(exponent, mantissa)
    val nan = isNaN(exponent, mantissa)
    (zero, inf, nan)
  }

  // Round to nearest (even)
  def roundMantissa(mantissa: UInt, expBits: Int, mantBits: Int): UInt = {
    val roundedMantissa = Wire(UInt((mantBits + 1).W)) // extra bit for rounding
    //val shiftAmount = Wire(UInt(expBits.W))

    // Calculate the round bit
    val roundBit = mantissa(mantBits)
    val stickyBit = Wire(Bool())
    stickyBit := mantissa(mantBits - 1, 0).orR

    // Round to nearest (even) logic
    roundedMantissa := Mux(
      roundBit === 1.U && (stickyBit || mantissa(mantBits - 1) === 1.U),
      mantissa + 1.U,
      mantissa
    )

    // Adjust for overflow and rounding
    Mux(roundedMantissa(mantBits) === 1.U, roundedMantissa(mantBits - 1, 0), roundedMantissa)
  }


}


