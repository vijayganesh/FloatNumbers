package org.vricsa.floats

import chisel3._
import chisel3.util._
import floats.FixedPoint
import spire.std.int

class FixedMul extends Module  {
    val integerBits: Int = 8
    val fractionalBits: Int = 8
    override val desiredName = s"FixedMul_${integerBits}_${fractionalBits}"

    val io = IO(new Bundle {
        val a = Input(new FixedPoint(integerBits, fractionalBits))
        val b = Input(new FixedPoint(integerBits, fractionalBits))
        val result = Output(new FixedPoint(integerBits, fractionalBits))
        val overflow = Output(Bool())
    })

    // Separate integer and fractional parts
    

    // Integer multiplication (includes sign bits)
    val finalwidth = 2 * (integerBits + fractionalBits)
    val int_product = Wire(SInt(finalwidth.W))
    int_product := (io.a.value * io.b.value)

    // Fractional multiplication with double precision
    
    val startPos = finalwidth/2 + integerBits 
    val endPos = finalwidth/2 - fractionalBits 
    // Combine results maintaining proper scaling
    val product = Wire(SInt(((integerBits + fractionalBits + 1)).W))
    product := int_product(startPos, endPos).asSInt

    // Detect overflow by checking if the result fits in the target width
    val resultWidth = integerBits + fractionalBits
    val expectedSign = int_product(finalwidth - 1)
    
    // Check sign extension consistency by comparing upper bits range with sign bit
    val upperBits = int_product(finalwidth-1, startPos)
    val upperBitsMatch = Wire(Bool())
    upperBitsMatch := Mux(expectedSign === 1.U, upperBits === ((1.U << (finalwidth-startPos)) - 1.U),
                                                upperBits === 0.U)
    
    // Check overflow in the result's upper bits
    val hasSignificantBits = (product >> (resultWidth-1)).asUInt.orR
    
    // Overflow occurs when either:
    // 1. Sign bits are not consistent (not proper sign extension)
    // 2. There are significant bits in the overflow region
    io.overflow := !upperBitsMatch || hasSignificantBits
    
    // Result is the combined product
    io.result.value := product
    
    printf(p"Input a: ${Hexadecimal(io.a.value)} \n")
    printf(p"Input b: ${Hexadecimal(io.b.value)} \n")
    printf(p"Integer product: ${Binary(int_product)}\n")
    
    printf(p"Combined result: ${Hexadecimal(io.result.value)} (${Binary(io.result.value)})\n")
    printf(p"Overflow: ${io.overflow}\n\n")
    
    
}