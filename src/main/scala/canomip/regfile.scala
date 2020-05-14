/**
 * RISC-V Int Reg File
 * First commit by Tealer.Guo
 * 2020/05/03 - Build alu basic file and class - First commit
 */
package canomip.core

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class regfile(len: Int, sim_mode: Bool) extends Component {
    val io = new Bundle {
        // Input
        val i_rw_mode = in UInt(2 bits) // 0 _ int reg read, 1 _ int reg write, 10 _ csr reg red, 11 _ csr reg write
        val i_int_reg_addr_1 = in UInt(5 bits)
        val i_int_reg_addr_2 = in UInt(5 bits)
        val i_csr_reg_addr = in UInt(12 bits) // 2020/05/03 Machine Mode only

            // write data
        val i_int_reg_1_needwrite_data = in SInt(len bits) // data need write
        val i_int_reg_2_needwrite_data = in SInt(len bits)

        // Output
        val o_int_reg_1_afterread_data = out SInt(len bits) // data after read
        val o_int_reg_2_afterread_data = out SInt(len bits)
        val o_csr_reg_afterread_data = out SInt(len bits)
    }

    // Logic

    // initial reg or use fpga bram
    // if sim use this, if run with fpga use blackbox
    val int_reg_file = Vec(RegInit(S(0, len bits)), 32) // int reg file
    val csr_reg_file = Vec(RegInit(S(0, len bits)), 4096) // csr reg file

    // BlackBox
    // TODO : fpga blackbox
}

object regfile {
    def main(args: Array[String]) {
        // TODO : Sim
    }
}