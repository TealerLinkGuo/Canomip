/**
 * RISC-V Int Reg File
 * First commit by Tealer.Guo
 * 2020/05/03 - Build regfile basic file and class - First commit
 * 2020/05/15 - Build finish regfile and no sim, file complim - Tealer.Guo
 */
package canomip.core

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class regfile(len: Int, sim_mode: Boolean) extends Component {
    val io = new Bundle {
        // Input
        val i_rw_mode = in UInt(2 bits) // 00 _ int reg read, 01 _ int reg write, 10 _ csr reg red, 11 _ csr reg write
        val i_int_reg_addr_1 = in UInt(5 bits) // when write mode, this is write address
        val i_int_reg_addr_2 = in UInt(5 bits)
        val i_csr_reg_addr = in UInt(12 bits) // 2020/05/03 Machine Mode only
            // Ctr
        val i_inst_type = in UInt(16 bits) // instrution type

            // write data
        val i_int_reg_needwrite_data = in SInt(len bits) // data need write
        val i_csr_reg_needwrite_data = in SInt(len bits)

        // Output
        val o_int_reg_1_afterread_data = out SInt(len bits) // data after read
        val o_int_reg_2_afterread_data = out SInt(len bits)
        val o_csr_reg_afterread_data = out SInt(len bits)
    }

    if(sim_mode == false) {
        // initial reg or use fpga bram
        // if sim use this, if run with fpga use blackbox
        val int_reg_file = Vec(RegInit(S(0, len bits)), 32) // int reg file
        val csr_reg_file = Vec(RegInit(S(0, len bits)), 4096) // csr reg file group 1

        // Logic

        when(io.i_rw_mode === U"00") {
            // Int read
            io.o_int_reg_1_afterread_data := int_reg_file(io.i_int_reg_addr_1)
            io.o_int_reg_2_afterread_data := int_reg_file(io.i_int_reg_addr_2)
            // not use
            // io.o_csr_reg_afterread_data := S(0)
        } .elsewhen(io.i_rw_mode === U"01") {
            // Int write
            int_reg_file(io.i_int_reg_addr_1) := io.i_int_reg_needwrite_data
            // not use
            // io.o_int_reg_1_afterread_data := S(0)
            // io.o_int_reg_2_afterread_data := S(0)
            // io.o_csr_reg_afterread_data := S(0)
        } .elsewhen(io.i_rw_mode === U"10") {
            // csr reg read
            when(io.i_csr_reg_addr <= U"")
            io.o_csr_reg_afterread_data := csr_reg_file(io.i_csr_reg_addr)
            // not use
            // io.o_int_reg_1_afterread_data := S(0)
            // io.o_int_reg_2_afterread_data := S(0)
        } .elsewhen(io.i_rw_mode === U"11") {
            // csr reg write
            csr_reg_file(io.i_csr_reg_addr) := io.i_csr_reg_needwrite_data
            // not use
            // io.o_int_reg_1_afterread_data := S(0)
            // io.o_int_reg_2_afterread_data := S(0)
            // io.o_csr_reg_afterread_data := S(0)
        } .otherwise {
            // Illegal
            io.o_csr_reg_afterread_data := S(0)
            io.o_csr_reg_afterread_data := S(0)
            io.o_int_reg_2_afterread_data := S(0)
        }
    }

    // BlackBox
    // TODO : fpga blackbox
}

object regfile {
    def main(args: Array[String]) {
        // Binary to Dec
        def binaryToDecWithOutRecur(src: String): Long = {
            // Convert bin ro dec
            val finalSrc = src.replaceAll("_", "")
            // println(finalSrc)

		    if(!finalSrc.matches("[0|1]*")){
		    	println("[Decoder Sim] INVALID INPUT .")
		    	return 0
		    }

		    val tmp = finalSrc.reverse
		    var res: Long = 0

		    for(i <- 0 to tmp.length - 1){
		    	res += tmp.substring(i, i + 1).toLong * (1 << i)
		    }
		    return res
	    }

        SimConfig.withWave.compile(new regfile(32, false)).doSim { dut =>
            dut.clockDomain.forkStimulus(period = 10) // time period

            dut.io.i_rw_mode #= binaryToDecWithOutRecur("1")
            dut.io.i_int_reg_addr_1 #= 5
            dut.io.i_int_reg_needwrite_data #= 10086
            dut.clockDomain.waitSampling(1) // time wait
        }
    }
}