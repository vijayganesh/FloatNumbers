package org.vricsa
import chisel3._
//import chiseltest.RawTester.test
//import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import chisel3.simulator.scalatest.ChiselSim

import org.vricsa.Generators._
import org.vricsa.ieeeTest.ieeeTest
import chisel3.simulator.EphemeralSimulator._ // Import ChiselSim simulator
import svsim._


class MvmTest extends AnyFlatSpec with ChiselSim {
  val rows = 2
  val cols = 3
  val matrix = RandomMatrixVectorGenerator.generateRandomMatrix(rows, cols) *100.0
  val vector = RandomMatrixVectorGenerator.generateRandomVector(cols) *100.0

  val result = matrix * vector

  println(f" A = ${matrix} and B = ${vector} \n The Result is ${result}")
  val testValue = 0.0254
  val exp = 8
  val manti = 23
  val singleConvert = new con_float_Uint(exp, manti)

  val longValue = singleConvert.doubleToIEEE(testValue)
  println(f" for the input ${testValue*2} --> ${Hexadecimal(longValue.U)} and back = ${singleConvert.ieeeToDouble(longValue)}")
  val totalWidth = exp + manti + 1
  behavior of "IEEE 754 Floating Point"

  it should "perform addition and subtraction correctly" in {
    simulate(new ieeeTest(exp, manti)) { dut => 
     // val inA = new IEEEArthiOperation(exp, manti)
     // val inB = new IEEEArthiOperation(exp, manti)
     // inA := longValue.U
    //  inB := longValue.U

      for(i<- 0 until rows){
        for(j<- 0 until cols){
          val inA = matrix(i,j) * 100
          val inB = vector(j)
          val longA = singleConvert.doubleToIEEE(inA)
          val longB = singleConvert.doubleToIEEE(inB)
          val expAdd = inA + inB
          val expSub = inA - inB
          val expMul = inA * inB
          val expDiv = inA / inB

          dut.io.inA.poke(longA.U)
          dut.io.inB.poke(longB.U)
          val obtainedAdd = dut.io.outAdd.peek()
          val obtainedMul = dut.io.outMul.peek()
          val obtainedSub = dut.io.outSub.peek()
          val obtainedDiv = dut.io.outDiv.peek()

          dut.clock.step(1)
          println(f" The Add of  Exp = ${expAdd} and Got = ${singleConvert.ieeeToDouble(obtainedAdd.litValue.toLong)}")
          println(f" The Mul of  Exp = ${expMul} and Got = ${singleConvert.ieeeToDouble(obtainedMul.litValue.toLong)}")
          println(f" The Sub of  Exp = ${expSub} and Got = ${singleConvert.ieeeToDouble(obtainedSub.litValue.toLong)}")
          println(f" The Div of  Exp = ${expDiv} and Got = ${singleConvert.ieeeToDouble(obtainedDiv.litValue.toLong)}")
          println("")



        }


      }
      /*
      dut.io.inA.poke(longValue.U)

      dut.io.inB.poke(longValue.U)

      val mulab = dut.io.outMul.peek()
      val addab = dut.io.outAdd.peek()
      dut.clock.step(1)

      println(f" The Mul of  Exp = ${testValue * testValue} and Got = ${singleConvert.ieeeToDouble(mulab.litValue.toLong)}")
      println(f" The Add of  Exp = ${testValue + testValue} and Got = ${singleConvert.ieeeToDouble(addab.litValue.toLong)}")
      */

    }

  }
}


