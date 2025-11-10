package org.vricsa.floats //package floats

import chisel3._
import chisel3.util._



class ieeeHPAdd(val exp:Int, val mantissa : Int) extends  Module {

  val dataWidth = exp+mantissa+1
  override val desiredName = s"IEEE_${exp}_${mantissa}_add"
  val io = IO(new Bundle {
    val inAdd_a =  Input(Bits(dataWidth.W))
    val inAdd_b =  Input(Bits(dataWidth.W))
    val addResult = Output(Bits(dataWidth.W))
  })
  val sign_a = Wire(Bool())
  val sign_b = Wire(Bool())
  val exp_a = Wire(UInt(exp.W))
  val exp_b = Wire(UInt(exp.W))
  // Create a temp mantisa with additional 2 bits for buffer and shift process
  val manBuf = mantissa+2
  val manti_a = WireInit(0.U(manBuf.W))
  val manti_b = WireInit(0.U(manBuf.W))
  val fmanti_a = WireInit(0.U(manBuf.W))
  val fmanti_b = WireInit(0.U(manBuf.W))
  val smanti_a = WireInit(0.U(manBuf.W))
  val smanti_b = WireInit(0.U(manBuf.W))
  val shiftValue = WireInit(0.S(exp.W))
  val fadd = WireInit(0.U(manBuf.W))
  val fsign = WireInit(0.B)
  val fexp = WireInit(exp_a)
  val texp = WireInit(exp_a)
  val tadd = WireInit(fadd)
  val tmanti = WireInit(0.U(mantissa.W))
  val fmanti = WireInit(0.U(mantissa.W))
  sign_a := io.inAdd_a(dataWidth -1 )
  sign_b := io.inAdd_b(dataWidth - 1)
  exp_a := io.inAdd_a(dataWidth - 2,mantissa)
  exp_b := io.inAdd_b(dataWidth - 2 ,mantissa)
  manti_a := Cat(1.U(1.W),io.inAdd_a(mantissa - 1 ,0))
  manti_b := Cat(1.U(1.W),io.inAdd_b(mantissa - 1 ,0))
  // printf(p"The mantisa of b = 0x${Hexadecimal(Cat(1.U(1.W),io.inAdd_b(9,0)))} and b=0x${Hexadecimal(manti_b)}")
  shiftValue := (exp_a - exp_b).asSInt


  FloatingPointExceptions.init(exp, mantissa)
  val (isZeroA, isInfA, isNaNA) = FloatingPointExceptions.checkExceptions(io.inAdd_a)
  val (isZeroB, isInfB, isNaNB) = FloatingPointExceptions.checkExceptions(io.inAdd_b)


  //printf("The shiftValue=%d \n",shiftValue)
  when(sign_a ^ sign_b) {
    when(sign_a) {
      // Need to take twos complement
      smanti_a := (-manti_a)(manBuf - 1, 0)
      //  printf(p"\n \t -- The 2's Compl of input a= b${Binary(smanti_a)} for ${Binary(manti_a)} \n")
    }
      .otherwise {
        smanti_a := manti_a
      }
    when(sign_b) {
      smanti_b := (-manti_b)(manBuf - 1, 0)
      // printf(p"\n \t -- The 2's Compl of input b = b${Binary(smanti_b)} for ${Binary(manti_b)} \n")
    }.otherwise {
      smanti_b := manti_b
    }
  }
    .otherwise{
      smanti_a := manti_a
      smanti_b := manti_b
    }
  val regSmantiA = WireInit(0.U(manBuf.W))
  val regSmantiB = WireInit(0.U(manBuf.W))
  val regshiftValue = WireInit(0.S(exp.W))
  val regInputA = WireInit(VecInit(Seq.fill(4)(0.U(dataWidth.W))))
  val regInputB = WireInit(VecInit(Seq.fill(4)(0.U(dataWidth.W))))
  val regIsZeroAStage = WireInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val regIszeroBStage = WireInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val regisNanAStage = WireInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val regisNanBStage = WireInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val regisInfAStage = WireInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val regisInfBStage = WireInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val regSignA = WireInit(VecInit(Seq.fill(4)(0.B)))
  val regSignB = WireInit(VecInit(Seq.fill(4)(0.B)))

  val regExpa = WireInit(0.U(exp.W))
  val regExpb = WireInit(0.U(exp.W))

  regSmantiA := smanti_a
  regSmantiB := smanti_b
  regshiftValue := shiftValue
  regExpa := exp_a
  regExpb := exp_b
  regisInfAStage(0) := isInfA
  regisInfBStage(0) := isInfB
  regisNanAStage(0) := isNaNA
  regisNanBStage(0) := isNaNB
  regIsZeroAStage(0) := isZeroA
  regIszeroBStage(0) := isZeroB
  regSignA(0) := sign_a
  regSignB(0) := sign_b
  regInputA(0) := io.inAdd_a
  regInputB(0) := io.inAdd_b
// Stage 2 shifting
  when(regshiftValue < 0.S){
    // b is greater then a
    // Right shift a to match b
    //shiftValue := -shiftValue
    fmanti_a := (regSmantiA.asSInt >> (-regshiftValue).asUInt)(manBuf - 1,0)
    fmanti_b := regSmantiB(mantissa + 1,0)
    texp := regExpb
    fsign := sign_b
    //  printf(p"--> B is greater 0x${Binary(smanti_a)} after shift 0x${Binary(fmanti_a)} <--")
  }
    .otherwise{
      //  a is greater then shift b
      fmanti_a := regSmantiA(manBuf - 1,0) //smanti_a(manBuf - 1,0)
      fmanti_b := (regSmantiB.asSInt >> regshiftValue.asUInt)(manBuf - 1,0) // (smanti_b.asSInt >> shiftValue.asUInt)(manBuf - 1,0)
      texp := regExpa
      // printf(p"--> A is greater 0x${Binary(smanti_b)} after shift 0x${Binary(fmanti_b)} <--")
      fsign := sign_a
    }
  regisInfAStage(1) := regisInfAStage(0)
  regisInfBStage(1) := regisInfBStage(0)
  regisNanAStage(1) := regisNanAStage(0)
  regisNanBStage(1) := regisNanBStage(0)
  regIsZeroAStage(1) := regIsZeroAStage(0)
  regIszeroBStage(1) := regIszeroBStage(0)
  regSignA(1) := regSignA(0)
  regSignB(1) := regSignB(0)
  regInputA(1) := regInputA(0)
  regInputB(1) := regInputB(0)

// Stage 3 only addition

  val regtexp = WireInit(0.U(exp.W))
  regtexp := texp
  // Add the two values
  fadd := fmanti_a + fmanti_b


  // printf(p"\n The addition of a = ${Hexadecimal(fmanti_a)}+${Hexadecimal(fmanti_b)} = ${Hexadecimal(fadd)} \n")
  // Need to shift based on logics
  // Both have same sign and result +ve and 10.xxx right shift
  //                                    0.001xx left shift will you get 1.xx
  //both are different sign and result is x]0.001xx left shift and the result is positive
  //                                      x]1.xxx  result is negative take twos compliment and shift if required
  val nshift = WireInit(0.U((exp-1).W))
  regisInfAStage(2) := regisInfAStage(1)
  regisInfBStage(2) := regisInfBStage(1)
  regisNanAStage(2) := regisNanAStage(1)
  regisNanBStage(2) := regisNanBStage(1)
  regIsZeroAStage(2) := regIsZeroAStage(1)
  regIszeroBStage(2) := regIszeroBStage(1)
  regSignB(2) := regSignB(1)
  regSignA(2) := regSignA(1)
  regInputA(2) := regInputA(1)
  regInputB(2) := regInputB(1)

  // Stage 4

  //when(sign_a ^ sign_b){
  when(regSignA(2) ^ regSignB(2)){
      //printf("\n -+-+-+ Reached for Different sign --------- \n")
    //tadd:=0.U
    when(fadd(manBuf - 1)){
      fsign := 1.B
      tadd := -fadd
      // printf("\n \t\t ----Reached for 11th bit as 1 (10,11) = %b\n",fadd(11,10))
    }.otherwise{
     // when(regSignA(2) === 1.U){
     //   tadd := -fadd
     // }
       // .otherwise{
          tadd := fadd
       // }

      //fsign :=regSignA(2)
      //  printf("\n \t\t ----Reached for 11th bit as 0 (10,11) = %b \n",fadd(11,10))
    }
    // use priority encoder to find number of shift required
    // Acutal matissa + 1 eg 10 bit mantessa we are going to look at 11th bit location
    when(tadd(mantissa)){
      nshift := 0.U
    }.otherwise{
      nshift := PriorityEncoder(Reverse(tadd(mantissa - 1,0)).asUInt)+1.U}
    tmanti := (tadd(mantissa -1 ,0) << nshift)(mantissa -1,0)
    fexp := regtexp - nshift
    //  printf(p"The Prioriy Diff encoder value = $nshift and tmanti=0x${Hexadecimal(tmanti)}")
  }.otherwise{
    // Both has same sign
   //  printf("\n ------ Reached for same sign ++++++++ \n")
    tadd := fadd
   // printf("\n\t The (11,10) values =%b\n",fadd(11,10))

    when(tadd(mantissa + 1,mantissa) === 2.U || tadd(mantissa + 1,mantissa) ===3.U )
    {
      tmanti := (tadd >> 1.U)(mantissa - 1,0)
      fexp := regtexp + 1.U
    }
      .elsewhen(tadd(mantissa + 1,mantissa) === 1.U)
      {
        // Do nothing
        tmanti := tadd(mantissa - 1,0)
        fexp := regtexp
      }.otherwise{
        // Do the reverse calculation
        nshift := PriorityEncoder(Reverse(tadd(mantissa -1,0)).asUInt)+1.U
        tmanti := (tadd(mantissa - 1,0) << nshift)(mantissa -1 ,0)
        fexp := regtexp - nshift
     //   printf(p"Reached other section -> \n \t nshfift = $nshift and tmanti=0x${Hexadecimal(tmanti)} for $tadd(9,0) \n")
        //  printf(p"\n \t \t The texp=0x${texp} and after shifting = $fexp \n")
      }
    //fsign := regSignA(2) //0.B
    //tmanti := tadd(9,0)
  }

  //val roundedMantissa = FloatingPointExceptions.roundMantissa(tmanti(mantissa-1,0), exp, mantissa-1)
  fmanti := tmanti //roundedMantissa(mantissa-1,0)// roundedMantissa
  // printf(p"The mantisa Vaues are b=0x${Hexadecimal(fmanti)}, and a = 0x${Hexadecimal(fexp)} The add values are 0x${Hexadecimal(fmanti_a + fmanti_b)} \n");
  //println("THe Priority Encoder value ="+PriorityEncoder("b10000".U))

  //io.addResult := io.inAdd_a + io.inAdd_b0x9DE0 and Got 0x9de0
  // Check the condition before sending the result


  regisInfAStage(3) := regisInfAStage(2)
  regisInfBStage(3) := regisInfBStage(2)
  regisNanAStage(3) := regisNanAStage(2)
  regisNanBStage(3) := regisNanBStage(2)
  regIsZeroAStage(3) := regIsZeroAStage(2)
  regIszeroBStage(3) := regIszeroBStage(2)
  regSignA(3) := regSignA(2)
  regSignB(3) := regSignB(2)
  regInputA(3) := regInputA(2)
  regInputB(3) := regInputB(2)
  // Stage 4
/*
  when(isZeroA === 1.U){
    io.addResult := io.inAdd_b
  }
  .elsewhen(isZeroB === 1.U){
    io.addResult := io.inAdd_a
  }
    .else
  when(isInfA === 1.U){
      io.addResult := io.inAdd_a
    }
    .elsewhen(isInfB === 1.U){
      io.addResult := io.inAdd_b
    }
    .elsewhen(isNaNA === 1.U){
      io.addResult := io.inAdd_a
    }
    .elsewhen(isNaNB === 1.U){
      io.addResult := io.inAdd_b
    }*/

  when(regisInfAStage(3) === 1.U || regisInfBStage(3) ===1.U ){
    io.addResult := FloatingPointExceptions.generateInfinity(regSignA(3))
  }
    .elsewhen(regIszeroBStage(3) === 1.U ){
      io.addResult := regInputA(3)
    }
    .elsewhen(regIsZeroAStage(3) === 1.U){
      io.addResult := regInputB(3)
    }
    .otherwise{
      io.addResult := Cat(fsign,fexp,fmanti)
    }

}

