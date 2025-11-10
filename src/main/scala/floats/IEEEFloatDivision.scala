package org.vricsa.floats
//package floats
import chisel3._
import chisel3.util._
class IEEEFloatingPointDivide(val expBits: Int, val mantBits: Int) extends Module {
  val totalBits = expBits + mantBits + 1
  val io = IO(new Bundle {
    val inputA = Input(UInt(totalBits.W))  // Combined Sign + Exponent + Mantissa
    val inputB = Input(UInt(totalBits.W))  // Combined Sign + Exponent + Mantissa
    val result = Output(UInt(totalBits.W)) // Result combined Sign + Exponent + Mantissa
    val resultIsNaN = Output(Bool())
    val resultIsInf = Output(Bool())
    val resultIsZero = Output(Bool())
  })

  // Initialize the FloatingPointExceptions object with exponent and mantissa bits
  FloatingPointExceptions.init(expBits, mantBits)

  // Unpack inputs
  val (signA, exponentA, mantissaA) = FloatingPointExceptions.unpack(io.inputA)
  val (signB, exponentB, mantissaB) = FloatingPointExceptions.unpack(io.inputB)

  // Detect exceptions for both A and B
  val (isZeroA, isInfA, isNaNA) = FloatingPointExceptions.checkExceptions(io.inputA)
  val (isZeroB, isInfB, isNaNB) = FloatingPointExceptions.checkExceptions(io.inputB)

  // Handle NaN
  val isNaNResult = isNaNA || isNaNB || (isInfA && isInfB && signA =/= signB)

  // Handle Infinity
  val isInfResult = isInfA && !isZeroB || isInfB && isZeroA

  // Handle Zero
  val isZeroResult = (isZeroA && !isInfB) || (isZeroB && !isInfA) || (isZeroA && isZeroB)

  // Perform division
  val signQuotient = signA ^ signB
  val bias = ((1 << (expBits - 1)) - 1).U
  val exponentDifference = exponentA - exponentB + bias
  val mantissaQuotient = (mantissaA.asSInt / mantissaB.asSInt).asUInt

  // Normalize the result
  val mantissaBitsExtended = mantBits + 1
  val normalizedMantissa = FloatingPointExceptions.roundMantissa(mantissaQuotient, expBits, mantBits)
  val normalizedExponent = Mux(mantissaQuotient === 0.U, 0.U, exponentDifference)

  // Create the final result
  val resultSign = signQuotient
  val resultMantissa = normalizedMantissa
  val resultExponent = Mux(resultMantissa === 0.U, 0.U, normalizedExponent)

  io.result := Cat(resultSign, resultExponent, resultMantissa)
  io.resultIsNaN := isNaNResult
  io.resultIsInf := isInfResult
  io.resultIsZero := isZeroResult
}


class ieeeHPDiv(val exp:Int,val mantissa: Int)  extends  Module {
  //  parameter value calculation
  val dataWidth = exp+mantissa + 1
  val expMaxValue = (scala.math.pow(2,exp-1) - 1).round
  val maxMatissaBits = (mantissa+1)*2
  ///
  val io = IO(new Bundle() {
    val inADiv = Input(Bits(dataWidth.W))
    val inBDiv = Input(Bits(dataWidth.W))
    val resDiv = Output(Bits(dataWidth.W))

  })
  // Declaration of Wires
  val sign_a = WireInit(0.B)
  val sign_b = WireInit(0.B)
  val fsign = WireInit(0.B)

  val exp_a = Wire(UInt(exp.W))
  val exp_b = Wire(UInt(exp.W))
  val fexp = WireInit(0.U(exp.W))

  val manti_a = WireInit(0.U(maxMatissaBits.W))
  val manti_b = WireInit(0.U((mantissa + 1).W))
  val tDiv = WireInit(0.U((mantissa+2).W))
  val fDiv = WireInit(0.U(mantissa.W))

  // Basic Assignment

  sign_a := io.inADiv(dataWidth -1)
  sign_b := io.inBDiv(dataWidth - 1)

  // fsign = sign(a) ^ sign(b)
  fsign := sign_a ^ sign_b
  exp_a := io.inADiv(dataWidth - 2,mantissa)
  exp_b := io.inBDiv(dataWidth - 2,mantissa)
  // fexp = exp_a + exp_b -127


  val tman = WireInit(0.U((maxMatissaBits - 2).W))
  tman := io.inADiv(mantissa - 1,0) << mantissa.U
  manti_a := Cat(1.U,tman)
  manti_b := Cat(1.U,io.inBDiv(mantissa -1 ,0))

  tDiv := manti_a / manti_b
  //printf("The Multiplication value is %b",tmul)
  when(tDiv(mantissa + 1)){
    fDiv := (tDiv >> 1)(mantissa -1,0)
    fexp := exp_a - exp_b +expMaxValue.U + 1.U
    //printf("\n The Exponent value in 1x.xx =%b for Ea=%b and Eb=%b\n",fexp,exp_a,exp_b)

  }.elsewhen(tDiv(mantissa)){
      // No need to shift
      fDiv := tDiv(mantissa -1,0)
      fexp := exp_a - exp_b + expMaxValue.U //15.U
      //printf("\n The Exponent value in 01.xx =%b for Ea=%b and Eb=%b\n",fexp,exp_a,exp_b)
    }

    .otherwise{
      //  printf("Should not reach here ")

      fDiv := (tDiv << 1)(mantissa - 1,0) // Need to verify for all values
      fexp := exp_a - exp_b + expMaxValue.U - 1.U //15.U -1.U
      // printf("\n a=%b / b=%b ",manti_a,manti_b)
      // printf("\n Div Value = %b and After Shifting = %b",tDiv,fDiv)
      // printf("\n The Exponent value in 00.xx =%b for Ea=%b and Eb=%b\n",fexp,exp_a,exp_b)
    }

  val (isZeroA, isInfA, isNaNA) = FloatingPointExceptions.checkExceptions(io.inADiv)
  val (isZeroB, isInfB, isNaNB) = FloatingPointExceptions.checkExceptions(io.inBDiv)

  when( isZeroB === 1.U ){
    io.resDiv := FloatingPointExceptions.generateInfinity(sign_a)
  }
    .elsewhen(isZeroA === 1.U){
      io.resDiv := io.inADiv

    }
    .elsewhen( isInfB === 1.U && isInfA =/= 1.U){
      io.resDiv := 0.U //FloatingPointExceptions.generateInfinity(sign_a)
    }

    .elsewhen(isNaNA === 1.U || isNaNB === 1.U ){
      io.resDiv := FloatingPointExceptions.generateNaN()
    }
    .otherwise{
      io.resDiv := Cat(fsign,fexp,fDiv)
    }

  // Final Result

}