/**
 * RISC-V Anlogic EG4 BRAM 64-bits BlackBox
 * Data len - 64 bits | Address len - 12 bits
 * First commit by Tealer.Guo
 * 2020/05/16 - Build BlackBox basic file and build finish - First commit
 * 2020/05/25 - Fix doc bugs - Tealer.Guo
 */
package canomip.fpgablackbox.anlogicEg4

import spinal.core._

class EG4_csr_reg_bram_64 extends BlackBox {
    val io = new Bundle {
        // Clock
        val clka = in Bool() // clock input
        // Input
        val wea = in Bool() // read/write Ctrl
        val cea = in Bool() // module clock enable
        val dia = in SInt(64 bits) // data need writ
        val addra = in UInt(12 bits) // bram address
        // Output
        val doa = out SInt(64 bits) // data read output
    }

    mapCurrentClockDomain(io.clka) // map clock to cur clock domain
    noIoPrefix()
    addRTLPath("./src/main/scala/canomip/fpga_blackbox/alogic_eg4/verilog/csr/EG4_csr_reg_bram_64.v") // megaFunc
}

object EG4_csr_reg_bram_64 {
    def main(args: Array[String]) {
        // NULL
    }
}