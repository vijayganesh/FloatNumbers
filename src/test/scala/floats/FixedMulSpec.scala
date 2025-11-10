package floats

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.simulator.scalatest.ChiselSim
import chisel3.simulator.EphemeralSimulator._
import scala.util.Random
import org.vricsa.floats.FixedMul

class FixedMulSpec extends AnyFlatSpec with ChiselSim {
  
  // Fixed-point format configuration
  val fractionalBits = 8
  val integerBits = 8
  
  // Create a converter with fixed number of fractional bits
  val converter = FixedPointConversion(fractionalBits)
  import converter.{toFixed, fromFixed}
 
  behavior of "FixedPoint Multiplication"

  it should "perform multiplication with positive numbers" in {
    simulate(new FixedMul()) { dut =>
      val testCases = Seq(
        (1.5, 2.0, 3.0),     // Simple multiplication
        (2.5, 2.0, 5.0),     // Whole numbers
        (0.5, 4.0, 2.0),     // Multiplication by 0.5
        (0.25, 0.5, 0.125),  // Small numbers
        (3.5, 2.0, 7.0)      // Larger numbers
      )

      for ((a, b, expected) <- testCases) {
        val aFixed = toFixed(a)
        val bFixed = toFixed(b)
        dut.io.a.value.poke(aFixed.S)
        dut.io.b.value.poke(bFixed.S)
        dut.clock.step()  // Allow one clock cycle for the print to occur
        
        val result = fromFixed(dut.io.result.value.peek().litValue)
        println(f"\n=== Test Case: $a%.3f × $b%.3f ===")
        println(f"Input a (fixed-point): $aFixed (${aFixed.toString(2)})")
        println(f"Input b (fixed-point): $bFixed (${bFixed.toString(2)})")
        println(f"Result (decimal): $result%.6f (expected: $expected%.6f)")
        println(f"Result (fixed-point): ${dut.io.result.value.peek().litValue}")
        println(f"Overflow: ${dut.io.overflow.peek().litValue}")
        println("=" * 40)
        
        assert(math.abs(result - expected) < 0.01, 
               f"Multiplication failed for $a * $b: got $result, expected $expected")
        assert(dut.io.overflow.peek().litValue == 0, 
               f"Unexpected overflow for $a * $b")
      }
    }
  }

  it should "handle negative numbers correctly" in {
    simulate(new FixedMul()) { dut =>
      val testCases = Seq(
        (-2.0, 3.0, -6.0),    // Negative * Positive
        (2.0, -3.0, -6.0),    // Positive * Negative
        (-2.0, -3.0, 6.0),    // Negative * Negative
        (-0.5, 2.0, -1.0),    // Negative fraction
        (-1.5, -2.0, 3.0)     // Negative fractions
      )

      for ((a, b, expected) <- testCases) {
        dut.io.a.value.poke(toFixed(a).S)
        dut.io.b.value.poke(toFixed(b).S)
        dut.clock.step()
        val result = fromFixed(dut.io.result.value.peek().litValue)
        assert(math.abs(result - expected) < 0.01, 
               f"Multiplication failed for $a * $b: got $result, expected $expected")
        assert(dut.io.overflow.peek().litValue == 0, 
               f"Unexpected overflow for $a * $b")
      }
    }
  }

  it should "detect overflow conditions" in {
    simulate(new FixedMul()) { dut =>
      val maxValue = FixedPointConversion.maxValue(integerBits, fractionalBits)
      val nearMax = maxValue * 0.9  // 90% of max value
      
      val overflowTestCases = Seq(
        (nearMax, 2.0),     // Multiplication causing overflow
        (maxValue, 1.5),    // Overflow with fraction
        (-maxValue, -1.5)   // Overflow with negative numbers
      )

      for ((a, b) <- overflowTestCases) {
        dut.io.a.value.poke(toFixed(a).S)
        dut.io.b.value.poke(toFixed(b).S)
        dut.clock.step()
        assert(dut.io.overflow.peek().litValue == 1, 
               f"Expected overflow not detected for $a * $b")
      }
    }
  }

  it should "handle very small numbers accurately" in {
    simulate(new FixedMul()) { dut =>
      val minValue = FixedPointConversion.minPositiveValue(fractionalBits)
      val testCases = Seq(
        (minValue, 2.0, minValue * 2.0),
        (minValue, minValue, 0.0),  // Result too small to represent
        (0.125, 0.125, 0.015625)
      )

      for ((a, b, expected) <- testCases) {
        dut.io.a.value.poke(toFixed(a).S)
        dut.io.b.value.poke(toFixed(b).S)
        
        val result = fromFixed(dut.io.result.value.peek().litValue)
        assert(math.abs(result - expected) < Math.pow(2, -8), 
               f"Small number multiplication failed for $a * $b: got $result, expected $expected")
      }
    }
  }

  it should "handle random test cases" in {
    simulate(new FixedMul()) { dut =>
      val rand = new Random(42)  // Fixed seed for reproducibility
      val maxValue = Math.pow(2, 3)  // Leave room to avoid overflow
      
      for (_ <- 1 to 30) {  // Run 30 random tests
        val a = (rand.nextDouble() * maxValue) * (if (rand.nextBoolean()) 1 else -1)
        val b = (rand.nextDouble() * maxValue) * (if (rand.nextBoolean()) 1 else -1)
        val expected = a * b
        
        dut.io.a.value.poke(toFixed(a).S)
        dut.io.b.value.poke(toFixed(b).S)
        dut.clock.step()
        val result = fromFixed(dut.io.result.value.peek().litValue)
        assert(math.abs(result - expected) < 0.08, 
               f"Random multiplication failed for $a * $b: got $result, expected $expected")
      }
    }
  }
}