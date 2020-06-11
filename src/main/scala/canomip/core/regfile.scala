/**
 * RISC-V Int Reg File
 * First commit by Tealer.Guo
 * 2020/05/03 - Build regfile basic file and class - First commit
 * 2020/05/15 - Build finish regfile and no sim, file complim - Tealer.Guo
 * 2020/05/16 - Fix bugs and sim success with sim_mode false - Tealer.Guo
 * 2020/05/25 - Add new mode - Tealer.Guo
 * 2020/06/06 - Add csr protect - Tealer.Guo
 */
package canomip.core

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import canomip.fpgablackbox.anlogicEg4._ // blackBox package

class regfile(len: Int, sim_mode: Boolean) extends Component {
    val io = new Bundle {
        // Input
        val i_rw_mode = in UInt(2 bits) // 00 _ int reg read, 01 _ int reg write, 10 _ csr reg red, 11 _ csr reg write
        val i_int_reg_addr_1 = in UInt(5 bits) // when write mode, this is write address
                                                // i_int_reg_addr = 0, read only
        val i_int_reg_addr_2 = in UInt(5 bits)
        val i_csr_reg_addr = in UInt(12 bits) // 2020/05/03 Machine Mode only, 2020/06/06 All privileged mode
                                              // i_csr_reg_addr[11:10] 00/01/10 - read/write, 11 - read only
            // Ctrl
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
        //////////////////////////////////////////////
        // use with computer sim, not suit for fpga //
        //////////////////////////////////////////////

        // initial reg or use fpga bram
        // if sim use this, if run with fpga use blackbox
        val int_reg_file = Vec(RegInit(S(0, len bits)), 32) // int reg file, initial value is 0
        val csr_reg_file = Mem(SInt(len bits), wordCount = 4096) // csr reg file

        ////////////////////////////////////////////////
        // Test setting remove before use             //
        val test_addr_csr = U"110000001111"        //
        csr_reg_file(test_addr_csr) := S(666)      //
        ////////////////////////////////////////////////

        // Logic

        when(io.i_rw_mode === U"00") {
            // Int read
            io.o_int_reg_1_afterread_data := int_reg_file(io.i_int_reg_addr_1)
            io.o_int_reg_2_afterread_data := int_reg_file(io.i_int_reg_addr_2)
            // not use
            io.o_csr_reg_afterread_data := S(0)
        } .elsewhen(io.i_rw_mode === U"01") {
            // Int write
            when(io.i_int_reg_addr_1 > U(0)) {
                // protect the zero reg
                int_reg_file(io.i_int_reg_addr_1) := io.i_int_reg_needwrite_data
            } .otherwise {
                // Illegal
                int_reg_file(io.i_int_reg_addr_1) := S(0, len bits) // write zero
            }
            // not use
            io.o_int_reg_1_afterread_data := S(0)
            io.o_int_reg_2_afterread_data := S(0)
            io.o_csr_reg_afterread_data := S(0)
        } .elsewhen(io.i_rw_mode === U"10") { // CSR
            // csr reg read
            io.o_csr_reg_afterread_data := csr_reg_file(io.i_csr_reg_addr)
            // not use
            io.o_int_reg_1_afterread_data := S(0)
            io.o_int_reg_2_afterread_data := S(0)
        } .elsewhen(io.i_rw_mode === U"11") { // CSR
            // csr reg write
            when(io.i_csr_reg_addr(11 downto 10) =/= U"11") {
                // not read only
                csr_reg_file(io.i_csr_reg_addr) := io.i_csr_reg_needwrite_data
            } .otherwise {
                // Illegal
                csr_reg_file(io.i_csr_reg_addr) := csr_reg_file(io.i_csr_reg_addr)
            }
            // not use
            io.o_int_reg_1_afterread_data := S(0)
            io.o_int_reg_2_afterread_data := S(0)
            io.o_csr_reg_afterread_data := S(0)
        } .otherwise {
            // Illegal
            io.o_csr_reg_afterread_data := S(0)
            io.o_int_reg_1_afterread_data := S(0)
            io.o_int_reg_2_afterread_data := S(0)
        }
    } else if(sim_mode == true) {
        //////////////////////////////////
        // suit for fpga, use fpga bram //
        //////////////////////////////////

        // initial BlackBox
        val eg4_bram_csr = new EG4_csr_reg_bram // 32-bits bram
        // val eg4_bram_csr = new EG4_csr_reg_bram_64 // 64-bits bram

        // initial Int Reg File
        val int_reg_file = Vec(RegInit(S(0, len bits)), 32) // int reg file, initial value is 0
        // Logic
        when(io.i_rw_mode === U"00") {
            // Int read
            io.o_int_reg_1_afterread_data := int_reg_file(io.i_int_reg_addr_1)
            io.o_int_reg_2_afterread_data := int_reg_file(io.i_int_reg_addr_2)
            // not use
            io.o_csr_reg_afterread_data := S(0)
        } .elsewhen(io.i_rw_mode === U"01") {
            // Int write
            when(io.i_int_reg_addr_1 > U(0)) {
                // protect the zero reg
                int_reg_file(io.i_int_reg_addr_1) := io.i_int_reg_needwrite_data
            } .otherwise {
                // Illegal
                int_reg_file(io.i_int_reg_addr_1) := S(0, len bits) // write zero
            }
            // not use
            io.o_int_reg_1_afterread_data := S(0)
            io.o_int_reg_2_afterread_data := S(0)
            io.o_csr_reg_afterread_data := S(0)
        } .elsewhen(io.i_rw_mode === U"10") { // CSR
            // csr reg read
            eg4_bram_csr.io.wea := False // bram read mode
            eg4_bram_csr.io.cea := True // bram clock enable
            eg4_bram_csr.io.addra := io.i_csr_reg_addr // bram address
            io.o_csr_reg_afterread_data := eg4_bram_csr.io.doa // data out
            // not use
            eg4_bram_csr.io.dia := S(0) // bram io not use
            io.o_int_reg_1_afterread_data := S(0)
            io.o_int_reg_2_afterread_data := S(0)
        } .elsewhen(io.i_rw_mode === U"11") { // CSR
            // csr reg write
            eg4_bram_csr.io.wea := True // bram write mode
            eg4_bram_csr.io.cea := True // bram clock enable
            eg4_bram_csr.io.addra := io.i_csr_reg_addr // bram address
            when(io.i_csr_reg_addr(11 downto 10) =/= U"11") {
                // not read only
                eg4_bram_csr.io.dia := io.i_csr_reg_needwrite_data // bram data in
            } .otherwise {
                // Illegal , read only
                val rabbish = 0
            }
            // not use
            io.o_int_reg_1_afterread_data := S(0)
            io.o_int_reg_2_afterread_data := S(0)
            io.o_csr_reg_afterread_data := S(0)
        } .otherwise {
            // Illegal
            io.o_csr_reg_afterread_data := S(0)
            io.o_int_reg_1_afterread_data := S(0)
            io.o_int_reg_2_afterread_data := S(0)
        }
    }
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

            // write int reg 5 - 10086
            dut.io.i_rw_mode #= binaryToDecWithOutRecur("01")
            dut.io.i_int_reg_addr_1 #= 5
            dut.io.i_int_reg_needwrite_data #= 10086
            dut.clockDomain.waitSampling(1) // time wait

            // read int reg 5
            dut.io.i_rw_mode #= binaryToDecWithOutRecur("00")
            dut.io.i_int_reg_addr_1 #= 5
            dut.clockDomain.waitSampling(1) // time wait
            assert(dut.io.o_int_reg_1_afterread_data.toLong == 10086)

            // write csr reg 2049 - 12345
            dut.io.i_rw_mode #= binaryToDecWithOutRecur("11")
            dut.io.i_csr_reg_addr #= 2049
            dut.io.i_csr_reg_needwrite_data #= 12345
            dut.clockDomain.waitSampling(1) // time wait

            // read csr reg 2049
            dut.io.i_rw_mode #= binaryToDecWithOutRecur("10")
            dut.io.i_csr_reg_addr #= 2049
            dut.clockDomain.waitSampling(1) // time wait
            assert(dut.io.o_csr_reg_afterread_data.toLong == 12345)

            // write csr
            dut.io.i_rw_mode #= binaryToDecWithOutRecur("11")
            dut.io.i_csr_reg_addr #= binaryToDecWithOutRecur("110000001111")
            dut.io.i_csr_reg_needwrite_data #= 10001
            dut.clockDomain.waitSampling(1) // time wait

            dut.io.i_rw_mode #= binaryToDecWithOutRecur("10")
            dut.io.i_csr_reg_addr #= binaryToDecWithOutRecur("110000001111")
            dut.clockDomain.waitSampling(1) // time wait
            assert(dut.io.o_csr_reg_afterread_data.toLong == 666)
        }
    }
}