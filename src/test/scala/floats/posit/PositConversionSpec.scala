package floats.posit

import org.scalatest._
import flatspec._
import matchers._
import org.vricsa.Generators.con_float_Uint
import svsim.CommonCompilationSettings.Timescale.Unit.s

class PositConversionSpec extends AnyFlatSpec with should.Matchers {

  private def approxEqual(a: Double, b: Double, relTol: Double = 0.05, absTol: Double = 1e-12): Boolean = {
    if (a.isNaN || b.isNaN) false
    else if (a.isInfinite || b.isInfinite) a == b
    else {
      val diff = math.abs(a - b)
      diff <= math.max(absTol, relTol * math.max(math.abs(a), math.abs(b)))
    }
  }

  "doubleToPosit and positToDouble" should "round-trip representative values for 8-bit posit (es=2)" in {
    val conv = new con_float_Uint(8, 23)
    val n = 8
    val es = 2

    val values = Seq(0.0, 1.25, -1.0, 2.0, 0.5, Math.PI, -100.0, 1e-6, 1e6)

    for (v <- values) {
      val pbits = conv.doubleToPosit(v, n, es)
      println(s" The pbits = $pbits")
      // bit-length should not exceed n
      pbits.bitLength should be <= n

      val back = conv.positToDouble(pbits, n, es)
      println(s" The back = $back" )

      if (v == 0.0) {
        back shouldEqual 0.0
      } else {
        withClue(s"value=$v, pbits=$pbits, back=$back") {
          assert(approxEqual(v, back, relTol = 0.20, absTol = 1e-6), s"Round-trip out of tolerance for $v -> $back")
        }
      }
    }
  }

  it should "encode zero to 0 and decode to 0" in {
    val conv = new con_float_Uint(8, 23)
    val p = conv.doubleToPosit(0.0, 8, 2)
    p shouldEqual BigInt(0)
    conv.positToDouble(p, 8, 2) shouldEqual 0.0
  }
}
