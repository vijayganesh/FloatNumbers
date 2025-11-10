package org.vricsa.floats
// package floats
import chisel3._
import chisel3.util._
object IEEEArthiOperation {
 // Helper function to create a fully initialized instance

 def apply(expWidth: Int, mantWidth: Int) : IEEEArthiOperation ={
  val bundle = Wire(new IEEEArthiOperation(expWidth, mantWidth))
  bundle.value := 0.U
  /*
  bundle.isNan := false.B
  bundle.isInf := false.B
  bundle.isZero := false.B
  */

  bundle

 }

 def apply(expWidth: Int, mantWidth: Int, DataValue: UInt): IEEEArthiOperation = {
  val bundle = Wire(new IEEEArthiOperation(expWidth, mantWidth))
  bundle.value := DataValue  // Assign mantissa value
  bundle
 }
}

class IEEEArthiOperation (val exp:Int, val mantissa: Int) extends  Bundle{


 // val width = exp+mantissa+1
 val totalwidth = (exp + mantissa) +1
  val value = UInt(totalwidth.W)
 /*
 val isNan = Bool()
 val isInf = Bool()
 val isZero = Bool()

  */

//  value := 0.U


 def assign(assValue : UInt): Unit = {
  FloatingPointExceptions.init(exp, mantissa)
  this.value := assValue
 // val (z,n,f) = FloatingPointExceptions.checkExceptions(assValue
   /*
  this.isZero := z
  this.isInf := f
  this.isNan := n

    */
 }
/*
 def :=(that: IEEEArthiOperation): Unit = {
  this.value := that.value
 }
*/

 /*
 def :=(value : UInt): IEEEArthiOperation={
  val temp = Wire(new IEEEArthiOperation(exp,mantissa))
  temp.value := value
  temp.isZero := false.B
  temp.isNan := false.B
  temp.isInf := false.B
  temp
 }
*/
 /*
 def apply(aVal: UInt): IEEEArthiOperation= {
  val bundle = Wire(new IEEEArthiOperation(exp,mantissa))
  bundle.value := aVal
  bundle
 // this.value := aVal
  // this
 }
*/

 def +(that: IEEEArthiOperation): IEEEArthiOperation = {

  val result = Wire(new IEEEArthiOperation(exp, mantissa))
  result.assign(0.U)
  //val addmodule = Module(new IEEEFloatingPointAdd(exp,mantissa))
  val addmodule = Module(new ieeeHPAdd(exp,mantissa))
  addmodule.io.inAdd_a := this.value
  addmodule.io.inAdd_b := that.value
 // addmodule.io.inputA := this.value
 // addmodule.io.inputB := that.value
  result.value := addmodule.io.addResult
 // result.isZero := addmodule.io.resultIsZero
 // result.isInf := addmodule.io.resultIsInf
 // result.isNan := addmodule.io.resultIsNaN
  //printf(" \n The added  Value is %d",result.value)
  result
 }

 def -(that: IEEEArthiOperation): IEEEArthiOperation = {

  val result = Wire(new IEEEArthiOperation(exp, mantissa))
  val addmodule = Module(new ieeeHPAdd(exp,mantissa))
  addmodule.io.inAdd_a := this.value
  // Change only the sign rest should be same
  addmodule.io.inAdd_b := Cat(~that.value(totalwidth-1),that.value(totalwidth-2,0))// ~that.value
  result.value := addmodule.io.addResult
  /*
  val (z,n,f) = FloatingPointExceptions.checkExceptions(addmodule.io.addResult)
  result.isZero := z
  result.isInf := f
  result.isNan := n
*/
  result
 }

 def *(that: IEEEArthiOperation): IEEEArthiOperation = {

  val result = Wire(new IEEEArthiOperation(exp, mantissa))
  // result.value := 0.U
 // result.isNan := false.B
  val mulmodule = Module(new ieeeHPMul(exp,mantissa))


  //mulmodule.io.inputA := 0.U
 // mulmodule.io.inputB := 0.U
  mulmodule.io.inputA := this.value

  // addmodule.io.inputA := this.value
  mulmodule.io.inputB := that.value
  result.value := mulmodule.io.result
/*
  val (z,n,f) = FloatingPointExceptions.checkExceptions(mulmodule.io.result)
  result.isZero := z
  result.isInf := f
  result.isNan :=

 */
  //result.isZero := mulmodule.io.resultIsZero
  //result.isInf := mulmodule.io.resultIsInf
  // result.isNan := mulmodule.io.resultIsNaN
  //printf(" \n The Multiplication  Value is %d \n",result.value)
  result
 }

 def /(that: IEEEArthiOperation): IEEEArthiOperation = {

  val result = Wire(new IEEEArthiOperation(exp, mantissa))
  val mulmodule = Module(new ieeeHPDiv(exp,mantissa))


  //mulmodule.io.inputA := 0.U
  // mulmodule.io.inputB := 0.U
  mulmodule.io.inADiv := this.value

  // addmodule.io.inputA := this.value
  mulmodule.io.inBDiv := that.value
  result.value := mulmodule.io.resDiv
/*
  //val (z,n,f) = FloatingPointExceptions.checkExceptions(mulmodule.io.resDiv)
  result.isZero := z
  result.isInf := f
  result.isNan := n

 */
  result
 }

 def >(that: IEEEArthiOperation): Bool = {

 val (sign1,exp1,mantissa1) = FloatingPointExceptions.unpack(this.value)
  val (sign2,exp2,mantissa2) = FloatingPointExceptions.unpack(that.value)
  val result = Wire(UInt(1.W))

  // Logic
  when(sign1 =/= sign2) {
  result:=  !sign1.asBool && sign2.asBool  // If in1 is positive and in2 is negative
  }.elsewhen(sign1 === 1.U) {
   // Both numbers are negative
   result := (exp1 < exp2) || (exp1 === exp2 && mantissa1 < mantissa2)
  }.otherwise {
   // Both numbers are positive
   result := (exp1 > exp2) || (exp1 === exp2 && mantissa1 > mantissa2)
  }

  result.asBool
 }

 def <(that: IEEEArthiOperation): Bool = {

  val (sign1,exp1,mantissa1) = FloatingPointExceptions.unpack(this.value)
  val (sign2,exp2,mantissa2) = FloatingPointExceptions.unpack(that.value)
  val result = Wire(UInt(1.W))

  when(sign1 =/= sign2) {
   result := sign1.asBool && !sign2  // If in1 is negative and in2 is positive
  }.elsewhen(sign1 === 1.U) {
   // Both numbers are negative
   result :=  (exp1 > exp2) || (exp1 === exp2 && mantissa1 > mantissa2)
  }.otherwise {
   // Both numbers are positive
   result :=  (exp1 < exp2) || (exp1 === exp2 && mantissa1 < mantissa2)
  }
  result.asBool
 }

 def >=(that: IEEEArthiOperation): Bool = {

  (this> that) || (this.value === that.value)
 }

 def <=(that: IEEEArthiOperation): Bool = {

  (this < that) || (this.value === that.value)
 }

 def ==(that: IEEEArthiOperation): Bool = {

  this.value === that.value
 }



}
