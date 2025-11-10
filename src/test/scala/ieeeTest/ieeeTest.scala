package org.vricsa
package ieeeTest
import chisel3._
import org.vricsa.floats.IEEEArthiOperation

class ieeeTest (val exp: Int, val manti: Int) extends Module {
  val Width = exp+manti+1
  val io = IO(new Bundle{

    val inA = Input(UInt(Width.W))
    val inB = Input(UInt(Width.W))
    val outAdd = Output(UInt(Width.W))
    val outMul = Output(UInt(Width.W))
    val outSub = Output(UInt(Width.W))
    val outDiv = Output(UInt(Width.W))
    //val inA = Input(new  IEEEArthiOperation(exp,manti))
    //val inB = Input(new  IEEEArthiOperation(exp,manti))
    //val outAdd = Output(new  IEEEArthiOperation(exp,manti))
    //val outMul = Output(new  IEEEArthiOperation(exp,manti))

  })

 // io.outAdd.assign(0.U)
 // io.outMul.assign(0.U)
  val inA = Wire(new  IEEEArthiOperation(exp,manti))
  val inB = Wire(new  IEEEArthiOperation(exp,manti))

  inA.assign(io.inA)
  inB.assign(io.inB)
 // inA := io.inA
 // inB := io.inB

 // io.outMul := inA * inB
  val addValue = inA + inB
  val mulValue = inA * inB
  val subValue = inA - inB
  val divValue = inA/inB

 // dontTouch(mulValue.isInf)
 // dontTouch(mulValue.isNan)
 // dontTouch(mulValue.isZero)
  io.outAdd := addValue.value
  io.outMul := mulValue.value
  io.outSub := subValue.value
  io.outDiv := divValue.value

 // printf(" The addvalue is %d ",addValue.value)


}
