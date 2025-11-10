package floats

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import chisel3.simulator.scalatest.ChiselSim

import chisel3.simulator.EphemeralSimulator._ // Import ChiselSim simulator
import scala.util.Random
import svsim._

class FixedPointSpec extends AnyFlatSpec with ChiselSim {
  
  // Test configurations
  val integerBits = 8    // Including sign bit
  val fractionalBits = 8 // 8 bits for fractional part
  
  def toFixed(d: Double): BigInt = {
    val scale = Math.pow(2, fractionalBits)
    BigInt((d * scale).toLong)
  }
  
  def fromFixed(x: BigInt): Double = {
    val scale = Math.pow(2, fractionalBits)
    x.toDouble / scale
  }
 
  behavior of "FixedPoint Arithmetic"

  it should "perform addition with positive numbers" in {

    simulate(new FixedPointArithmetic(integerBits, fractionalBits)) { dut =>
      val testCases = Seq(
        (1.5, 2.25, 3.75),    // Simple positive addition
        (3.25, 4.75, 8.0),    // Larger numbers
        (0.125, 0.875, 1.0),  // Small numbers
        (15.5, 16.25, 31.75)  // Near max positive value
      )

      for ((a, b, expected) <- testCases) {
        dut.io.a.value.poke(toFixed(a).S)
        dut.io.b.value.poke(toFixed(b).S)
        dut.io.operation.poke(0.U) // ADD operation
        
        val result = fromFixed(dut.io.result.value.peek().litValue)
        assert(math.abs(result - expected) < 0.01, 
               f"Addition failed for $a + $b: got $result, expected $expected")
        assert(dut.io.overflow.peek().litValue == 0, 
               f"Unexpected overflow for $a + $b")
      }
    }
  }

  it should "handle negative numbers in addition" in {
    
    simulate(new FixedPointArithmetic(integerBits, fractionalBits)) { dut =>
      val testCases = Seq(
        (-1.5, 2.25, 0.75),     // Negative + Positive
        (-3.25, -4.75, -8.0),   // Negative + Negative
        (-0.125, 0.875, 0.75),  // Small negative + Positive
        (-15.5, -16.0, -31.5)   // Large negative numbers
      )

      for ((a, b, expected) <- testCases) {
        dut.io.a.value.poke(toFixed(a).S)
        dut.io.b.value.poke(toFixed(b).S)
        dut.io.operation.poke(0.U) // ADD operation
        
        val result = fromFixed(dut.io.result.value.peek().litValue)
        assert(math.abs(result - expected) < 0.01, 
               f"Addition failed for $a + $b: got $result, expected $expected")
        assert(dut.io.overflow.peek().litValue == 0,
               f"Unexpected overflow for $a + $b")
      }
    }
  }

  it should "detect overflow in addition" in {
    simulate(new FixedPointArithmetic(integerBits, fractionalBits)) { dut =>
      val maxVal = Math.pow(2, integerBits - 1) - Math.pow(2, -fractionalBits)
      val testCases = Seq(
        (maxVal, 1.0),           // Positive overflow
        (-maxVal - 1, -1.0),     // Negative overflow
        (maxVal/2, maxVal/2 + 1) // Near overflow
      )

      for ((a, b) <- testCases) {
        dut.io.a.value.poke(toFixed(a).S)
        dut.io.b.value.poke(toFixed(b).S)
        dut.io.operation.poke(0.U) // ADD operation
        assert(dut.io.overflow.peek().litValue == 1,
               f"Expected overflow not detected for $a + $b")
      }
    }
  }

  it should "perform multiplication correctly" in {
    simulate(new FixedPointArithmetic(integerBits, fractionalBits)) { dut =>
      val testCases = Seq(
        (1.5, 2.0, 3.0),      // Basic multiplication
        (0.5, 0.5, 0.25),     // Fractional multiplication
        (-2.0, 3.0, -6.0),    // Sign handling
        (0.125, 8.0, 1.0),    // Power of 2
        (-0.25, -4.0, 1.0)    // Negative * Negative
      )

      for ((a, b, expected) <- testCases) {
        dut.io.a.value.poke(toFixed(a).S)
        dut.io.b.value.poke(toFixed(b).S)
        dut.io.operation.poke(2.U) // MUL operation
        
        val result = fromFixed(dut.io.result.value.peek().litValue)
        assert(math.abs(result - expected) < 0.01, 
               f"Multiplication failed for $a * $b: got $result, expected $expected")
        assert(dut.io.overflow.peek().litValue == 0,
               f"Unexpected overflow for $a * $b")
      }
    }
  }
}