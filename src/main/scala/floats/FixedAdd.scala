package org.vricsa.floats

import chisel3._
import chisel3.util._
import floats.FixedPoint

class FixedAdd(integerBits: Int, fractionalBits: Int) extends Module {
    override val desiredName = s"FixedAdd_${integerBits}_${fractionalBits}"
    
    val io = IO(new Bundle {
        val a = Input(new FixedPoint(integerBits, fractionalBits))
        val b = Input(new FixedPoint(integerBits, fractionalBits))
        val result = Output(new FixedPoint(integerBits, fractionalBits))
        val overflow = Output(Bool())
    })

    // Total width needed for addition including overflow detection
    val widenedBits = (integerBits + fractionalBits + 1).W

    // Perform addition with overflow detection
    val sum = Wire(SInt(widenedBits))
    sum := io.a.value +& io.b.value  // +& operator includes carry bit

    // Detect overflow by comparing the top two bits
    // If they are different, we have an overflow
    io.overflow := sum(sum.getWidth-1) =/= sum(sum.getWidth-2)

    // Result is the lower bits, excluding the overflow bit
    io.result.value := sum(sum.getWidth-2, 0).asSInt
}
