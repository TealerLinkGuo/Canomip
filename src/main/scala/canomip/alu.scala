/**
 * RISC-V Int ALU
 * First commit by Tealer.Guo
 * 2020/04/18 - Build alu basic file and class - First commit
 * 2020/05/03 - Finish the alu module and sim - Tealer.Guo
 */
package canomip

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class alu(len: Int) extends Component {
    val io = new Bundle {
        // Input
        val i_inst_type = in UInt(16 bits) // instruction type code
        val i_rs1_data = in SInt(len bits) // reg 1 data
        val i_rs2_imm_data = in SInt(len bits) // reg 2 data or imm data after extended

        // Output
        val o_res_data = out SInt(len bits)
    }

    // Logic
    when(io.i_inst_type === U(28) || io.i_inst_type === U(19)) {
        // ADD, ADDI
        io.o_res_data := io.i_rs1_data + io.i_rs2_imm_data
    } .elsewhen(io.i_inst_type === U(29)) {
        // SUB
        io.o_res_data := io.i_rs1_data - io.i_rs2_imm_data
    } .elsewhen(io.i_inst_type === U(25) || io.i_inst_type === U(30)) {
        // SLL, SLLI
        if(len == 32) {
            when(U(io.i_rs2_imm_data(5)) === U"0") {
                io.o_res_data := io.i_rs1_data |<< U(io.i_rs2_imm_data(4 downto 0))
            } .otherwise {
                io.o_res_data := S(0)
            }
        } else if(len == 64) {
            io.o_res_data := io.i_rs1_data |<< U(io.i_rs2_imm_data(4 downto 0))
        }
    } .elsewhen(io.i_inst_type === U(20) || io.i_inst_type === U(31)) {
        // SLT. SLTI
        when(io.i_rs1_data < io.i_rs2_imm_data) {
            io.o_res_data := S(1)
        } .otherwise {
            io.o_res_data := S(0)
        }
    } .elsewhen(io.i_inst_type === U(32) || io.i_inst_type === U(21)) {
        // SLTU, SLTIU
        when(U(io.i_rs1_data) < U(io.i_rs2_imm_data)) {
            io.o_res_data := S(1)
        } .otherwise {
            io.o_res_data := S(0)
        }
    } .elsewhen(io.i_inst_type === U(22) || io.i_inst_type === U(33)) {
        // XOR, XORI
        io.o_res_data := io.i_rs1_data ^ io.i_rs2_imm_data
    } .elsewhen(io.i_inst_type === U(23) || io.i_inst_type === U(36)) {
        // OR, ORI
        io.o_res_data := io.i_rs1_data | io.i_rs2_imm_data
    } .elsewhen(io.i_inst_type === U(24) || io.i_inst_type === U(37)) {
        // AND, ANDI
        io.o_res_data := io.i_rs1_data & io.i_rs2_imm_data
    } .elsewhen(io.i_inst_type === U(27) || io.i_inst_type === U(35)) {
        // SRA, SRAI
        if(len == 32) {
            when(U(io.i_rs2_imm_data(5)) === U"0") {
                io.o_res_data := io.i_rs1_data >> U(io.i_rs2_imm_data(4 downto 0))
            } .otherwise {
                io.o_res_data := S(0)
            }
        } else if(len == 64) {
            io.o_res_data := io.i_rs1_data >> U(io.i_rs2_imm_data(4 downto 0))
        }
    } .elsewhen(io.i_inst_type === U(34) || io.i_inst_type === U(26)) {
        // SRL, SRLI
        if(len == 32) {
            io.o_res_data := io.i_rs1_data |>> U(io.i_rs2_imm_data(4 downto 0))
        } else if(len == 64) {
            io.o_res_data := io.i_rs1_data |>> U(io.i_rs2_imm_data(5 downto 0))
        }
    } .elsewhen(io.i_inst_type === U(191) || io.i_inst_type === U(281)) {
        // ADDW, ADDIW
        if(len == 64) {
            val tmp_addiw = io.i_rs1_data + io.i_rs2_imm_data
            when(U(tmp_addiw(31)) === U"1") {
                io.o_res_data := S(Cat(U"11111111111111111111111111111111", tmp_addiw(31 downto 0)))
            } .otherwise {
                io.o_res_data := S(Cat(U"00000000000000000000000000000000", tmp_addiw(31 downto 0)))
            }
        } else if(len == 32) {
            // Illegal
            io.o_res_data := S(0)
        }
    } .elsewhen(io.i_inst_type === U(291)) {
        // SUBW
        if(len == 64) {
            val tmp_subw = io.i_rs1_data - io.i_rs2_imm_data
            when(U(tmp_subw(31)) === U"1") {
                io.o_res_data := S(Cat(U"11111111111111111111111111111111", tmp_subw(31 downto 0)))
            } .otherwise {
                io.o_res_data := S(Cat(U"00000000000000000000000000000000", tmp_subw(31 downto 0)))
            }
        } else if(len == 32) {
            // Illegal
            io.o_res_data := S(0)
        }
    } .elsewhen(io.i_inst_type === U(251) || io.i_inst_type === U(301)) {
        // SLLW, SLLIW
        if(len == 64) {
            when(U(io.i_rs2_imm_data(4)) === U"0") {
                val tmp_slliw = io.i_rs1_data |<< U(io.i_rs2_imm_data)
                when(U(tmp_slliw(31)) === U"1") {
                    io.o_res_data := S(Cat(U"11111111111111111111111111111111", tmp_slliw(31 downto 0)))
                } .otherwise {
                    io.o_res_data := S(Cat(U"00000000000000000000000000000000", tmp_slliw(31 downto 0)))
                }
            } .otherwise {
                // Illegal
                io.o_res_data := S(0)
            }
        } else if(len == 32) {
            // Illegal
            io.o_res_data := S(0)
        }
    } .elsewhen(io.i_inst_type === U(261) || io.i_inst_type === U(341)) {
        // SRLW, SRLIW
        if(len == 64) {
            when(U(io.i_rs2_imm_data(4)) === U"0") {
                val tmp_srliw = io.i_rs1_data |>> U(io.i_rs2_imm_data)
                when(U(tmp_srliw(31)) === U"1") {
                    io.o_res_data := S(Cat(U"11111111111111111111111111111111", tmp_srliw(31 downto 0)))
                } .otherwise {
                    io.o_res_data := S(Cat(U"00000000000000000000000000000000", tmp_srliw(31 downto 0)))
                }
            } .otherwise {
                // Illegal
                io.o_res_data := S(0)
            }
        } else if(len == 32) {
            // Illegal
            io.o_res_data := S(0)
        }
    } .elsewhen(io.i_inst_type === U(271) || io.i_inst_type === U(351)) {
        // SRAW, SRAIW
        if(len == 64) {
            when(U(io.i_rs2_imm_data(4)) === U"0") {
                val tmp_sraiw = io.i_rs1_data >> U(io.i_rs2_imm_data)
                when(U(tmp_sraiw(31)) === U"1") {
                    io.o_res_data := S(Cat(U"11111111111111111111111111111111", tmp_sraiw(31 downto 0)))
                } .otherwise {
                    io.o_res_data := S(Cat(U"00000000000000000000000000000000", tmp_sraiw(31 downto 0)))
                }
            } .otherwise {
                // Illegal
                io.o_res_data := S(0)
            }
        } else if(len == 32) {
            // Illegal
            io.o_res_data := S(0)
        }
    } .otherwise {
        // Illegal
        io.o_res_data := S(0)
    }
}

object alu {
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

        // Tester
        SimConfig.withWave.doSim(new alu(32)) { dut =>
            // 32-bits
            dut.io.i_inst_type #= 28 // ADD
            dut.io.i_rs1_data #= 256
            dut.io.i_rs2_imm_data #= 1
            sleep(1)
            assert(dut.io.o_res_data.toLong == 257)

            dut.io.i_inst_type #= 19 // ADDI
            dut.io.i_rs1_data #= 256
            dut.io.i_rs2_imm_data #= 1
            sleep(1)
            assert(dut.io.o_res_data.toLong == 257)

            dut.io.i_inst_type #= 29 // SUB
            dut.io.i_rs1_data #= 256
            dut.io.i_rs2_imm_data #= 50
            sleep(1)
            assert(dut.io.o_res_data.toLong == 206)

            dut.io.i_inst_type #= 30 // SLL
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 << 3)

            dut.io.i_inst_type #= 25 // SLLI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 << 3)

            dut.io.i_inst_type #= 31 // SLT
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 0)

            dut.io.i_inst_type #= 20 // SLTI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 0)

            dut.io.i_inst_type #= 32 // SLTU
            dut.io.i_rs1_data #= -25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 0)

            dut.io.i_inst_type #= 21 // SLTI
            dut.io.i_rs1_data #= -25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 0)

            dut.io.i_inst_type #= 33 // XOR
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 26)

            dut.io.i_inst_type #= 22 // XORI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 26)

            dut.io.i_inst_type #= 34 // SRL
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 26 // SRLI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 35 // SRA
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 27 // SRAI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 36 // OR
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 27)

            dut.io.i_inst_type #= 23 // ORI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 27)

            dut.io.i_inst_type #= 37 // AND
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 1)

            dut.io.i_inst_type #= 24 // ANDI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 1)
        }

        SimConfig.withWave.doSim(new alu(64)) { dut =>
            // 32-bits
            dut.io.i_inst_type #= 28 // ADD
            dut.io.i_rs1_data #= 256
            dut.io.i_rs2_imm_data #= 1
            sleep(1)
            assert(dut.io.o_res_data.toLong == 257)

            dut.io.i_inst_type #= 19 // ADDI
            dut.io.i_rs1_data #= 256
            dut.io.i_rs2_imm_data #= 1
            sleep(1)
            assert(dut.io.o_res_data.toLong == 257)

            dut.io.i_inst_type #= 29 // SUB
            dut.io.i_rs1_data #= 256
            dut.io.i_rs2_imm_data #= 50
            sleep(1)
            assert(dut.io.o_res_data.toLong == 206)

            dut.io.i_inst_type #= 30 // SLL
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 << 3)

            dut.io.i_inst_type #= 25 // SLLI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 << 3)

            dut.io.i_inst_type #= 31 // SLT
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 0)

            dut.io.i_inst_type #= 20 // SLTI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 0)

            dut.io.i_inst_type #= 32 // SLTU
            dut.io.i_rs1_data #= -25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 0)

            dut.io.i_inst_type #= 21 // SLTI
            dut.io.i_rs1_data #= -25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 0)

            dut.io.i_inst_type #= 33 // XOR
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 26)

            dut.io.i_inst_type #= 22 // XORI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 26)

            dut.io.i_inst_type #= 34 // SRL
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 26 // SRLI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 35 // SRA
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 27 // SRAI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 36 // OR
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 27)

            dut.io.i_inst_type #= 23 // ORI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 27)

            dut.io.i_inst_type #= 37 // AND
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 1)

            dut.io.i_inst_type #= 24 // ANDI
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 1)

            // 64-bits
            dut.io.i_inst_type #= 191 // ADDIW
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 28)

            dut.io.i_inst_type #= 281 // ADDW
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 28)

            dut.io.i_inst_type #= 291 // SUBW
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 22)

            dut.io.i_inst_type #= 251 // SLLIW
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 << 3)

            dut.io.i_inst_type #= 301 // SLLW
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 << 3)

            dut.io.i_inst_type #= 261 // SRLIW
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 341 // SRLIW
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 271 // SRAIW
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)

            dut.io.i_inst_type #= 351 // SRAIW
            dut.io.i_rs1_data #= 25
            dut.io.i_rs2_imm_data #= 3
            sleep(1)
            assert(dut.io.o_res_data.toLong == 25 >> 3)
        }
    }
}