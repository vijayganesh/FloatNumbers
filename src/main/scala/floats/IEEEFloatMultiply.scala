package org.vricsa.floats //package floats
import chisel3._
import chisel3.util._
class IEEEFloatingPointMultiply(val expBits: Int, val mantBits: Int) extends Module {
  val totalBits = expBits + mantBits + 1
  override val desiredName = s"IEEE_${expBits}_${mantBits}_mul"
  val io = IO(new Bundle {
    val inputA = Input(UInt(totalBits.W))  // Combined Sign + Exponent + Mantissa
    val inputB = Input(UInt(totalBits.W))  // Combined Sign + Exponent + Mantissa
    val result = Output(UInt(totalBits.W)) // Result combined Sign + Exponent + Mantissa
    val resultIsNaN = Output(Bool())
    val resultIsInf = Output(Bool())
    val resultIsZero = Output(Bool())
  })

  io.result := 0.U
  io.resultIsZero := false.B
  io.resultIsInf := false.B
  io.resultIsNaN := false.B
  // Initialize the FloatingPointExceptions object with exponent and mantissa bits
  FloatingPointExceptions.init(expBits, mantBits)
// val m = io.inputA
  // Unpack inputs
  val (signA, exponentA, mantissaA) = FloatingPointExceptions.unpack(io.inputA)
  val (signB, exponentB, mantissaB) = FloatingPointExceptions.unpack(io.inputB)

  // Detect exceptions for both A and B
  val (isZeroA, isInfA, isNaNA) = FloatingPointExceptions.checkExceptions(io.inputA)
  val (isZeroB, isInfB, isNaNB) = FloatingPointExceptions.checkExceptions(io.inputB)

  // Handle NaN
  val isNaNResult = isNaNA || isNaNB || (isInfA && isInfB && signA =/= signB)

  // Handle Infinity
  val isInfResult = (isInfA || isInfB) && !isNaNResult && !(isInfA && isInfB && signA =/= signB)

  // Handle Zero
  val isZeroResult = (isZeroA || isZeroB) //|| (isZeroA && !isInfB && !isNaNB) || (isZeroB && !isInfA && !isNaNA)

  // Perform multiplication
  val signProduct = signA ^ signB
  val bias = ((1 << (expBits - 1)) - 1).U

  val exponentSum = exponentA + exponentB - bias
  val mantissaProduct = (mantissaA.asSInt * mantissaB.asSInt).asUInt

  // Normalize the result
  val mantissaBitsExtended = mantBits + 1
  val normalizedMantissa = FloatingPointExceptions.roundMantissa(mantissaProduct, expBits, mantBits)
  val normalizedExponent = exponentSum + mantissaBitsExtended.U

  // Create the final result
  val resultSign = signProduct
  val resultMantissa = normalizedMantissa
  val resultExponent = Mux(resultMantissa === 0.U, 0.U, normalizedExponent)

  io.result := Cat(resultSign, resultExponent, resultMantissa)
  io.resultIsNaN := isNaNResult
  io.resultIsInf := isInfResult
  io.resultIsZero := isZeroResult
}


class ieeeHPMul(val exp:Int,val mantissa: Int) extends  Module {
  val dataWidth = exp+mantissa + 1
  val expMaxValue = (scala.math.pow(2,exp-1) - 1).round
  val io = IO(new Bundle() {
    val inputA = Input(UInt(dataWidth.W))
    val inputB = Input(UInt(dataWidth.W))
    val result = Output(UInt(dataWidth.W))

  })

  FloatingPointExceptions.init(exp, mantissa)

  val (signA, exponentA, mantissaA) = FloatingPointExceptions.unpack(io.inputA)
  val (signB, exponentB, mantissaB) = FloatingPointExceptions.unpack(io.inputB)

  // Detect exceptions for both A and B
  val (isZeroA, isInfA, isNaNA) = FloatingPointExceptions.checkExceptions(io.inputA)
  val (isZeroB, isInfB, isNaNB) = FloatingPointExceptions.checkExceptions(io.inputB)

  // Handle NaN
  val isNaNResult = isNaNA || isNaNB || (isInfA && isInfB && signA =/= signB)

  // Handle Infinity
  val isInfResult = (isInfA || isInfB) && !isNaNResult && !(isInfA && isInfB && signA =/= signB)

  // Handle Zero
  val isZeroResult = (isZeroA && isZeroB) || (isZeroA && !isInfB && !isNaNB) || (isZeroB && !isInfA && !isNaNA)
  //
  // Declaration of Wires
  val sign_a = WireInit(0.B)
  val sign_b = WireInit(0.B)
  val fsign = RegInit(0.B)

  val exp_a = Wire(UInt(exp.W))
  val exp_b = Wire(UInt(exp.W))
  val fexp = RegInit(0.U(exp.W))
  val maxMatissaBits = (mantissa+1)*2
  val manti_a = WireInit(0.U((mantissa+1).W))
  val manti_b = WireInit(0.U((mantissa+1).W))
  val tmul = WireInit(0.U((maxMatissaBits).W))
  // Store result Mantissa
  val fmul = RegInit(0.U(mantissa.W))

  // Basic Assignment

  sign_a := io.inputA(dataWidth -1)
  sign_b := io.inputB(dataWidth -1)

  // fsign = sign(a) ^ sign(b)
  //fsign := sign_a ^ sign_b
  exp_a := io.inputA(dataWidth -2,mantissa)
  exp_b := io.inputB(dataWidth - 2 ,mantissa)
  // fexp = exp_a + exp_b -127
  val expWidth = exp



  manti_a := Cat(1.U,io.inputA(mantissa - 1,0))
  manti_b := Cat(1.U,io.inputB(mantissa - 1,0))
  val isZeroAStage = RegInit(VecInit(Seq.fill(3)(0.U(1.W))))
  val isZeroBStage = RegInit(VecInit(Seq.fill(4)(0.U(1.W))))

  val isInfBStage = RegInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val isInfAStage = RegInit(VecInit(Seq.fill(4)(0.U(1.W))))

  val isNanAStage = RegInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val isNanBStage = RegInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val regSignaState = RegInit(0.U(1.W))
  val regSignaState1 = RegInit(0.U(1.W))
  val regSignaState2 = RegInit(0.U(1.W))
  val regSignbState = RegInit(0.U(1.W))
  val regSignbState1 = RegInit(0.U(1.W))
  val regSignbState2 = RegInit(0.U(1.W))
  val regExpAStage = RegInit(0.U(expWidth.W))
  val regExpBStage = RegInit(0.U(expWidth.W))
  val regExpAStage1 = RegInit(0.U(expWidth.W))
  val regExpBStage1 = RegInit(0.U(expWidth.W))
  val regMantiAStage = RegInit(0.U((mantissa+1).W))
  val regMantiBStage =   RegInit(0.U((mantissa+1).W))

  regSignaState := sign_a
  regSignbState := sign_b
  regExpAStage := exp_a
  regExpBStage := exp_b
  regMantiAStage := manti_a
  regMantiBStage := manti_b
  isInfAStage(0) := isInfA
  isInfBStage(0) := isInfB
  isZeroAStage(0) := isZeroA
  isZeroBStage(0) := isZeroB
  isNanAStage(0) := isNaNA
  isNanBStage(0) := isNaNB



// stage 2 Only Multiplication computation
  //tmul := manti_a * manti_b

  val Regtmul = RegInit(0.U((maxMatissaBits).W))


  Regtmul := regMantiBStage * regMantiAStage
  regExpBStage1 := regExpBStage
  regExpAStage1 := regExpAStage
  isInfAStage(1) := isInfAStage(0)
  isInfBStage(1) := isInfBStage(0)
  isZeroAStage(1) := isZeroAStage(0)
  isZeroBStage(1) := isZeroBStage(0)
  isNanAStage(1) :=  isNanAStage(0)
  isNanBStage(1) := isNanBStage(0)
  regSignaState1 := regSignaState
  regSignbState1 := regSignbState
  //printf("The Multiplication value is %b",tmul)
  when(Regtmul(maxMatissaBits -1)){
    // for 10 bit mantissa (19,10)
    // 20 +2 result 19-10 gives 10 bit value remaining discarded
    fmul := (Regtmul >> 1)(maxMatissaBits - 3,mantissa)
    // need to update the 15.u with corresponding values
    fexp := regExpAStage1 +& regExpBStage1 -expMaxValue.U + 1.U
   // printf("\n The Exponent value in 1x.xx =%b for Ea=%b and Eb=%b and Mul = %b\n",fexp,exp_a,exp_b,fmul)

  } // looking at location 22 -2 value
    .elsewhen(Regtmul(maxMatissaBits - 2 )){

      // No need to shift
      fmul := Regtmul(maxMatissaBits - 3,mantissa)
      // 15 ->
      fexp := regExpAStage1 +& regExpBStage1 -expMaxValue.U
      //printf("\n The Exponent value in 01.xx =%d for Ea=%d and Eb=%d and Mul = %b Maxexp = %d cal = %d\n",fexp,exp_a,exp_b,fmul,expMaxValue.asUInt, (regExpAStage1 +& regExpBStage1) )
    }

    .otherwise{
      // printf("Should not reach here ")
      fmul := 0.U
      // expmaxvalue 2^exp -1
      fexp := regExpAStage1 +& regExpBStage1 -expMaxValue.U
    }

  isInfAStage(2) := isInfAStage(1)
  isInfBStage(2) := isInfBStage(1)
  isZeroAStage(2) := isZeroAStage(1)
  isZeroBStage(2) := isZeroBStage(1)
  isNanAStage(2) :=  isNanAStage(1)
  isNanBStage(2) := isNanBStage(1)
  regSignaState2 := regSignaState1
  regSignbState2 := regSignbState1
  //val (isZeroA, isInfA, isNaNA) = FloatingPointExceptions.checkExceptions(io.inputA)
  //val (isZeroB, isInfB, isNaNB) = FloatingPointExceptions.checkExceptions(io.inputB)
  //val ina = io.inputA
  //val inb = io.inputB
  fsign := regSignbState2 ^ regSignaState2

  val regResult = RegInit(0.U(dataWidth.W))
  // Final result
  when(isZeroAStage(2) === 1.U || isZeroBStage(2) === 1.U){
    regResult := 0.U
  }
    .elsewhen(isInfAStage(2) === 1.U || isInfBStage(2) === 1.U){
      regResult := FloatingPointExceptions.generateInfinity(regSignaState2.asBool)
    }

    .elsewhen(isNanAStage(2) === 1.U || isNanBStage(2) === 1.U){
      regResult := FloatingPointExceptions.generateNaN()
    }
    .otherwise{
      regResult := Cat(fsign,fexp,fmul)
    }


  // Final Result
  io.result := regResult


}
