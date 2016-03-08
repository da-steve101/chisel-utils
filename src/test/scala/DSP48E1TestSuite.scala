/*
 This file tests the Xilinx DSP48E1
 
 */

import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._
import chiselutils.xilinx._

class DSP48E1TestSuite extends TestSuite {

  @Test def testAdd {
    class Add extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, width=16)
        val b = UInt(INPUT, width=16)
        val out = UInt(OUTPUT, width=16)
      }
      val dspParams = new DSP48E1Params
      val dsp = Module( new DSP48E1(dspParams) )
      io.out := io.a + io.b
    }

    class AddTests(c : Add) extends Tester(c) {
      val a = BigInt(123)
      val b = BigInt(456)
      val out = a + b
      poke(c.io.a, a)
      poke(c.io.b, b)
      expect(c.io.out, out)
    }
    launchCppTester((c: Add) => new AddTests(c))
  }

  @Test def generateVerilog {
    class UserModule extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, width=30)
        val b = UInt(INPUT, width=18)
        val c = UInt(OUTPUT, width=48)
      }
      val dspParams = new DSP48E1Params
      val dspMod = Module( new DSP48E1(dspParams) )
      dspMod.io.a := io.a
      dspMod.io.b := io.b
      io.c := dspMod.io.p
      dspMod.io.rsta := this.reset
      dspMod.io.rstb := this.reset
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new UserModule))
  }
}
