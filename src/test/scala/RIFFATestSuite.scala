/*
 This file tests the RIFFA interface
 */

import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._
import chiselutils.interfaces._

class RIFFATestSuite extends TestSuite {

  @Test def testSerial {
    class UserModule extends Interfaceable[UInt, UInt](UInt(width=8), UInt(width=8)) {
      io.tx.bits := RegNext(io.rx.bits)
      io.tx.valid := RegNext(io.rx.valid)
      io.rx.ready := RegNext(io.tx.ready)
    }

    def getUserMod() : UserModule = { new UserModule }

    class RIFFAWithParam extends RIFFA[UInt, UInt, UserModule](32, 4, 4, getUserMod);

    class TimeSeriesTests(c : RIFFAWithParam) extends Tester(c) {
      val r = scala.util.Random

      val rxOn = true
      val rxLast = true
      val rxLen = 4
      val rxOff = 0
      val rxData = r.nextInt
      val rxDataValid = true

      val txAck = true
      val txReady = true

      poke(c.io.rxOn, Bool(rxOn).litValue())
      poke(c.io.rxLast, Bool(rxLast).litValue())
      poke(c.io.rxLen, BigInt(rxLen))
      poke(c.io.rxOff, BigInt(rxOff))
      poke(c.io.rxData, BigInt(rxData))
      poke(c.io.rxDataValid, Bool(rxDataValid).litValue())
      poke(c.io.txAck, Bool(txAck).litValue())
      poke(c.io.txReady, Bool(txReady).litValue())

      for ( i <- 0 until 10 ) {
        step(1) // step individual clocks?
        expect(c.io.txOn, Bool(true).litValue())
      }
    }

    launchCppTester((c : RIFFAWithParam) => new TimeSeriesTests(c))
  }
/*
  @Test def testVec {
    class UserModule extends Interfaceable[Vec[UInt], Vec[UInt]](Vec(UInt(width=8),4), Vec(UInt(width=8),4)) {
      io.tx.bits := RegNext(io.rx.bits)
      io.tx.valid := RegNext(io.rx.valid)
      io.rx.ready := RegNext(io.tx.ready)
    }

    class RIFFAWithParam extends RIFFA[Vec[UInt], Vec[UInt], UserModule](32, 1, 1, getUserMod);

    def getUserMod() : UserModule = { new UserModule }

    class TimeSeriesTests(c : RIFFAWithParam) extends Tester(c) {
  
      val a = BigInt(123)
      val b = BigInt(456)
      val out = a + b
      //poke(c.io.a, a)
      //poke(c.io.b, b)
      //expect(c.io.out, out)
    }

    launchCppTester((c : RIFFAWithParam) => new TimeSeriesTests(c))
  }
 */
}
