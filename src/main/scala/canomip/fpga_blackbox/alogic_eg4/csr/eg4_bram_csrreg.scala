/**
 * RISC-V Anlogic EG4 BRAM BlackBox
 * Data len - 32 bits | Address len - 12 bits
 * First commit by Tealer.Guo
 * 2020/05/04 - Build BlackBox basic file - First commit
 * 2020/05/16 - Build finish all module - Tealer.Guo
 * 2020/05/25 - Fix doc - Tealer.Guo
 */
package canomip.fpgablackbox.anlogicEg4

import spinal.core._

class EG4_csr_reg_bram extends BlackBox {
    val io = new Bundle {
        // Clock
        val clka = in Bool() // clock input
        // Input
        val wea = in Bool() // read/write Ctrl
        val cea = in Bool() // module clock enable
        val dia = in SInt(32 bits) // data need write
        val addra = in UInt(12 bits) // bram address
        // Output
        val doa = out SInt(32 bits) // data read output
    }

    mapCurrentClockDomain(io.clka) // map clock to cur clock domain
    noIoPrefix()
    addRTLPath("./src/main/scala/canomip/fpga_blackbox/alogic_eg4/verilog/csr/EG4_csr_reg_bram.v")
}

object EG4_csr_reg_bram {
    def main(args: Array[String]) {
        // NULL
    }
}