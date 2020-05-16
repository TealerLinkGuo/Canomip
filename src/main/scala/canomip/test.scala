// test file for develepment
// delete befor commit
// Tealer.Guo
package canomip.core

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import canomip.core.fpgablackbox.anlogicEg4._

class test extends Component {
    val io = new Bundle {
        val a = in SInt(64 bits)
        val b = out SInt(64 bits)
    }

    val abc = new EG4_csr_reg_bram_64
    // abc.io.clka := False
    abc.io.wea := False
    abc.io.cea := False
    abc.io.dia := io.a
    abc.io.addra := U(0)
    io.b := abc.io.doa
}

object test {
    def main(args: Array[String]) {
        val abcd = SpinalVerilog(new test)
        abcd.mergeRTLSource("mergeRTL")
    }
}