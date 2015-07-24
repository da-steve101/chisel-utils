/*
 This file is an interface to DSP48E1 defined at:
 http://www.xilinx.com/support/documentation/user_guides/ug479_7Series_DSP48E1.pdf

 NOTE: The following signals can only be directly connected to another DSP
 ACOUT  BCOUT  PCOUT  MULTSIGNOUT  CARRYSIGNOUT
   ^     ^       ^        ^             ^
 ACIN   BCIN   PCIN   MULTSIGNIN   CARRYSIGNIN
 */

package ChiselXilinx

import Chisel._

class DSP48E1IO extends Bundle {
  // Access to all pins except implied clock in order as in documentation
  val a              = Bits(INPUT, width=30);   a.nameIt("A", true)
  val acin           = Bits(INPUT, width=30);   acin.nameIt("ACIN", true)
  val acout          = Bits(OUTPUT, width=30);  acout.nameIt("ACOUT", true)
  val alumode        = Bits(INPUT, width=4);    alumode.nameIt("ALUMODE", true)
  val b              = Bits(INPUT, width=18);   b.nameIt("B", true)
  val bcin           = Bits(INPUT, width=18);   bcin.nameIt("BCIN", true)
  val bcout          = Bits(OUTPUT, width=18);  bcout.nameIt("BCOUT", true)
  val c              = Bits(INPUT, width=48);   c.nameIt("C", true)
  val carrycascin    = Bool(INPUT);             carrycascin.nameIt("CARRYCASCIN", true)
  val carrycascout   = Bool(INPUT);             carrycascout.nameIt("CARRYCASCOUT", true)
  val carryin        = Bool(INPUT);             carryin.nameIt("CARRYIN", true)
  val carryinsel     = Bits(INPUT, width=3);    carryinsel.nameIt("CARRYINSEL", true)
  val carryout       = Bits(OUTPUT, width=4);   carryout.nameIt("CARRYOUT", true)
  val cea1           = Bool(INPUT);             cea1.nameIt("CEA1", true)
  val cea2           = Bool(INPUT);             cea2.nameIt("CEA2", true)
  val cead           = Bool(INPUT);             cead.nameIt("CEAD", true)
  val cealumode      = Bool(INPUT);             cealumode.nameIt("CEALUMODE", true)
  val ceb1           = Bool(INPUT);             ceb1.nameIt("CEB1", true)
  val ceb2           = Bool(INPUT);             ceb2.nameIt("CEB2", true)
  val cec            = Bool(INPUT);             cec.nameIt("CEC", true)
  val cecarryin      = Bool(INPUT);             cecarryin.nameIt("CECARRYIN", true)
  val cectrl         = Bool(INPUT);             cectrl.nameIt("CECTRL", true)
  val ced            = Bool(INPUT);             ced.nameIt("CED", true)
  val ceinmode       = Bool(INPUT);             ceinmode.nameIt("CEINMODE", true)
  val cem            = Bool(INPUT);             cem.nameIt("CEM", true)
  val cep            = Bool(INPUT);             cep.nameIt("CEP", true)
  val d              = Bits(INPUT, width=25);   d.nameIt("D", true)
  val inmode         = Bits(INPUT, width=5);    inmode.nameIt("INMODE", true)
  val multsignin     = Bool(INPUT);             multsignin.nameIt("MULTSIGNIN", true)
  val multsignout    = Bool(OUTPUT);            multsignout.nameIt("MULTSIGNOUT", true)
  val opmode         = Bits(INPUT, width=7);    opmode.nameIt("OPMODE", true)
  val overflow       = Bool(OUTPUT);            overflow.nameIt("OVERFLOW", true)
  val p              = Bits(OUTPUT, width=48);  p.nameIt("P", true)
  val patternbdetect = Bool(OUTPUT);            patternbdetect.nameIt("PATTERNBDETECT", true)
  val patterndetect  = Bool(OUTPUT);            patterndetect.nameIt("PATTERNDETECT", true)
  val pcin           = Bits(INPUT, width=48);   pcin.nameIt("PCIN", true)
  val pcout          = Bits(OUTPUT, width=48);  pcout.nameIt("PCOUT", true)
  val rsta           = Bool(INPUT);             rsta.nameIt("RSTA", true)
  val rstallcarryin  = Bool(INPUT);             rstallcarryin.nameIt("RSTALLCARRYIN", true)
  val rstalumode     = Bool(INPUT);             rstalumode.nameIt("RSTALUMODE", true)
  val rstb           = Bool(INPUT);             rstb.nameIt("RSTB", true)
  val rstc           = Bool(INPUT);             rstc.nameIt("RSTC", true)
  val rstctrl        = Bool(INPUT);             rstctrl.nameIt("RSTCTRL", true)
  val rstd           = Bool(INPUT);             rstd.nameIt("RSTD", true)
  val rstinmode      = Bool(INPUT);             rstinmode.nameIt("RSTINMODE", true)
  val rstm           = Bool(INPUT);             rstm.nameIt("RSTM", true)
  val rstp           = Bool(INPUT);             rstp.nameIt("RSTP", true)
  val underflow      = Bool(OUTPUT);            underflow.nameIt("UNDERFLOW", true)
}

class DSP48E1Params( acascreg : Int = 1, adreg : Int = 1, alumodereg : Int = 1,
  areg : Int = 1, bcascreg : Int = 1, breg : Int = 1, carryinreg : Int = 1,
  carryinselreg : Int = 1, creg : Int = 1, dreg : Int = 1, inmodereg : Int = 1,
  mreg : Int = 1, opmodereg : Int = 1, preg : Int = 1, a_input : String = "DIRECT",
  b_input : String = "DIRECT", use_dport : Boolean = false, use_mult : String = "MULTIPLY",
  use_simd : String = "ONE48", autoreset_patdet : String = "NO_RESET", mask : BigInt = BigInt(0),
  pattern : BigInt = BigInt(0), sel_mask : String = "MASK", use_pattern_detect : Boolean = false ) extends VerilogParameters {

  // Helper methods
  def isZeroOrOne(x : Int) : Boolean = (x == 0 || x == 1)
  def isZeroOneTwo(x : Int) : Boolean = (x == 0 || x == 1 || x == 2)

  // Input Validation
  Predef.assert(acascreg >= 0 && acascreg <= areg, "ACASCREG must be less than AREG")
  Predef.assert(areg != 1 || acascreg == 1, "ACASCREG must be 1 if AREG is 1")
  Predef.assert(areg != 2 || acascreg != 0, "ACASCREG cant be 0 if AREG is 2")
  Predef.assert(isZeroOrOne(adreg), "ADREG must be 0 or 1")
  Predef.assert(isZeroOrOne(alumodereg), "ALUMODEREG must be 0 or 1")
  Predef.assert(isZeroOneTwo(areg), "AREG must be 0,1,2")
  Predef.assert(bcascreg >= 0 && bcascreg <= breg, "BCASCREG must be less than BREG")
  Predef.assert(isZeroOneTwo(breg), "BREG must be 0,1,2")
  Predef.assert(isZeroOrOne(carryinreg), "CARRYINREG must be 0 or 1")  
  Predef.assert(isZeroOrOne(carryinselreg), "CARRYINSELREG must be 0 or 1")  
  Predef.assert(isZeroOrOne(creg), "CREG must be 0 or 1")  
  Predef.assert(isZeroOrOne(dreg), "DREG must be 0 or 1")  
  Predef.assert(isZeroOrOne(inmodereg), "INMODEREG must be 0 or 1")  
  Predef.assert(isZeroOrOne(mreg), "MREG must be 0 or 1")  
  Predef.assert(isZeroOrOne(opmodereg), "OPMODEREG must be 0 or 1")  
  Predef.assert(isZeroOrOne(preg), "PREG must be 0 or 1")  
  Predef.assert(List("DIRECT", "CASCADE") contains a_input, "A_INPUT must be 'DIRECT' or 'CASCADE'")
  Predef.assert(List("DIRECT", "CASCADE") contains b_input, "B_INPUT must be 'DIRECT' or 'CASCADE'")
  Predef.assert(List("NONE", "MULTIPLY", "DYNAMIC") contains use_mult, "USE_MULT must be 'NONE', 'MULTIPLY', 'DYNAMIC'")
  Predef.assert(List("ONE48", "TWO24", "FOUR12") contains use_simd, "USE_SIMD must be 'ONE48', 'TWO24' or 'FOUR12'")
  Predef.assert(use_simd == "ONE48" || use_mult == "NONE", "USE_SIMD must be 'ONE48' for USE_MULT != 'NONE'")
  Predef.assert(List("NO_RESET", "RESET_MATCH", "RESET_NOT_MATCH") contains autoreset_patdet,
    "AUTORESET_PATDET must be 'NO_RESET', 'RESET_MATCH' or 'RESET_NOT_MATCH'")
  Predef.assert(List("MASK", "C", "ROUNDING_MODE1", "ROUNDING_MODE2") contains sel_mask,
    "SEL_MASK must be 'MASK', 'C', 'ROUNDING_MODE1', 'ROUNDING_MODE2'")

  // Parameters definition
  val ACASCREG = acascreg
  val ADREG = adreg
  val ALUMODEREG = alumodereg
  val AREG = areg
  val BCASCREG = bcascreg
  val BREG = breg
  val CARRYINREG = carryinreg
  val CARRYINSELREG = carryinselreg
  val CREG = creg
  val DREG = dreg
  val INMODEREG = inmodereg
  val MREG = mreg
  val OPMODEREG = opmodereg
  val PREG = preg
  val A_INPUT = a_input
  val B_INPUT = b_input
  val USE_DPORT = use_dport
  val USE_MULT = use_mult
  val USE_SIMD = use_simd
  val AUTORESET_PATDET = autoreset_patdet
  val MASK = mask
  val PATTERN = pattern
  val SEL_MASK = sel_mask
  val USE_PATTERN_DETECT = if ( use_pattern_detect ) "PATDET" else "NO_PATDET"
}

class DSP48E1( val dspParams : DSP48E1Params ) extends BlackBox {
  val io = new DSP48E1IO
  setVerilogParameters(dspParams)
  // clock has same name so no issue
  addClock(Driver.implicitClock)
  // remove implied resets as there are many defined as io
  this.reset = null
  this.resets.clear()

  // TODO: Implement DSP functionality for simulation

}
