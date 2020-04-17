/**
 * RISC-V instruction decoder
 * First Commit by Tealer.Guo
 * 2020/04/06 - Build decoder file and basic class - First Commit
 * 2020/04/15 - Build full RV32I/RV64I decoder logic but no putput, no sim - Tealer.Guo
 * 2020/04/16 - Fix some bugs and finish the RV64I decoder, still no sim - Tealer.Guo
 * 2020/04/17 - Code review, fix some bug and finish the decoder sim code - Tealer.Guo
 */
package canomip

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class decoder extends Component {
    val io = new Bundle {
        // Input
        val i_inst = in UInt(32 bits) // 定长32 bit instruction
        // Output
        val o_rs1 = out UInt(5 bits) // rs1
        val o_rs2 = out UInt(5 bits) // rs2
        val o_rd = out UInt(5 bits)  // rd

        val o_imm12 = out UInt(12 bits) // imm 12-bits
        val o_imm20 = out UInt(20 bits) // imm 20-bits

        val o_inst_type = out UInt(16 bits) // instruction type
    }
    
    // result Bundle
    case class decoder_res() extends Bundle {
        val inst_type = UInt(16 bits)
        val output_flag = UInt(3 bits) // 001 - R_type. 100 - I_type. 011 - S_type. 010 - B_type. 011 - U_type. 111 - J_type. 110 - ECALL/EBRAK
        val rs1 = UInt(5 bits) // Used as uimm in CSRRWI / CSRRSI / CSRRCI
        val rs2 = UInt(5 bits) // Used as shamt in SLLI / SRLI / SRAI
        val rd = UInt(5 bits)
        val imm12 = UInt(12 bits) // I_type, S_type and B_type, used as csr in CSR Load/Store Instructions, used as fm, pred, succ in FENCE
        val imm20 = UInt(20 bits) // U_type and J_type
    }

    // variable
    val opcode = io.i_inst(6 downto 0)
    val funct3 = io.i_inst(14 downto 12)
    val funct7 = io.i_inst(31 downto 25)

    // Bundle
    val inst_type_bundle = new decoder_res()

    // Logic
    switch(opcode) {
        // RV32 / RV64 Instructions

        is(U"0110011") { // RV32I Arithmetic Instruction
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := io.i_inst(24 downto 20)
            inst_type_bundle.output_flag := U"001"
            inst_type_bundle.imm12 := U(0)
            inst_type_bundle.imm20 := U(0)

            when(funct3 === U"000") {
                when(funct7 === U"0000000") { // ADD
                    inst_type_bundle.inst_type := U(28)
                } .elsewhen(funct7 === U"0100000") { // SUB
                    inst_type_bundle.inst_type := U(29)
                } .otherwise {
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .elsewhen(funct3 === U"001") {
                when(funct7 === U"0000000") { // SLL
                    inst_type_bundle.inst_type := U(30)
                } .otherwise {
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .elsewhen(funct3 === U"010") {
                when(funct7 === U"0000000") { // SLT
                    inst_type_bundle.inst_type := U(31)
                } .otherwise {
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .elsewhen(funct3 === U"011") {
                when(funct7 === U"0000000") { // SLTU
                    inst_type_bundle.inst_type := U(32)
                } .otherwise {
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .elsewhen(funct3 === U"100") {
                when(funct7 === U"0000000") { // XOR
                    inst_type_bundle.inst_type := U(33)
                } .otherwise {
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .elsewhen(funct3 === U"101") {
                when(funct7 === U"0000000") { // SRL
                    inst_type_bundle.inst_type := U(34)
                } .elsewhen(funct7 === U"0100000") { // SRA
                    inst_type_bundle.inst_type := U(35)
                } .otherwise {
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .elsewhen(funct3 === U"110") {
                when(funct7 === U"0000000") { // OR
                    inst_type_bundle.inst_type := U(36)
                } .otherwise {
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .elsewhen(funct3 === U"111") {
                when(funct7 === U"0000000") { // AND
                    inst_type_bundle.inst_type := U(37)
                } .otherwise {
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .otherwise { // Illegal
                inst_type_bundle.inst_type := U(0)
            }
        }

        is(U"0010011") {
            // Immediate Arithmetic Instructions
            // normal instruction
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U"100"
            inst_type_bundle.imm12 := io.i_inst(31 downto 20)
            inst_type_bundle.imm20 := U(0)
            inst_type_bundle.inst_type := U(0)

            // special I_type instruction
            when(funct3 === U"001" && funct7 === U"0000000") { // SLLI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt
                inst_type_bundle.output_flag := U"001"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(25)
            } 
            .elsewhen(funct3 === U"101" && funct7 === U"0000000") { // SRLI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt
                inst_type_bundle.output_flag := U"001"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(26)
            }
            .elsewhen(funct3 === U"101" && funct7 === U"0100000") { // SRAI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt
                inst_type_bundle.output_flag := U"001"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(27)
            } 
            .elsewhen(funct3 === U"000") { // ADDI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(19)
            }
            .elsewhen(funct3 === U"010") { // SLTI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(20)
            }
            .elsewhen(funct3 === U"011") { // SLTIU
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(21)
            }
            .elsewhen(funct3 === U"100") { // XORI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(22)
            }
            .elsewhen(funct3 === U"110") { // ORI
                 inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(23)
            }
            .elsewhen(funct3 === U"111") { // ANDI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(24)
            }
            .otherwise { // Illegal
                 inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U(0)
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(0)
            }
        }

        is(U"0100011") {
            // Store Instructions
            inst_type_bundle.rd := U(0)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := io.i_inst(24 downto 20)
            inst_type_bundle.output_flag := U"011"
            inst_type_bundle.imm12 := U(Cat(io.i_inst(31 downto 25), io.i_inst(11 downto 7)))
            inst_type_bundle.imm20 := U(0)

            when(funct3 === U"000") { // SB
                inst_type_bundle.inst_type := U(16)
            }
            .elsewhen(funct3 === U"001") { // SH
                inst_type_bundle.inst_type := U(17)
            }
            .elsewhen(funct3 === U"010") { // SW
                inst_type_bundle.inst_type := U(18)
            }
            .elsewhen(funct3 === U"011") { // SD(RV64I)
                inst_type_bundle.inst_type := U(112)
            }
            .otherwise { // Illegal
                inst_type_bundle.inst_type := U(0)
            }
        }

        is(U"0000011") {
            // Load Instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U"100"
            inst_type_bundle.imm12 := io.i_inst(31 downto 20)
            inst_type_bundle.imm20 := U(0)

            when(funct3 === U"000") { // LB
                inst_type_bundle.inst_type := U(11)
            }
            .elsewhen(funct3 === U"001") { // LH
                inst_type_bundle.inst_type := U(12)
            }
            .elsewhen(funct3 === U"010") { // LW
                inst_type_bundle.inst_type := U(13)
            }
            .elsewhen(funct3 === U"100") { // LBU
                inst_type_bundle.inst_type := U(14)
            }
            .elsewhen(funct3 === U"101") { // LHU
                inst_type_bundle.inst_type := U(15)
            }
            .elsewhen(funct3 === U"110") { // LWU(RV64I)
                inst_type_bundle.inst_type := U(110)
            }
            .elsewhen(funct3 === U"011") { // LD(RV64I)
                inst_type_bundle.inst_type := U(111)
            }
            .otherwise { // Illegal
                inst_type_bundle.inst_type := U(0)
            }
        }

        is(U"1100011") {
            // Branch Instructions
            inst_type_bundle.rd := U(0)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := io.i_inst(24 downto 20)
            inst_type_bundle.output_flag := U"010"
            val b_type_imm_tmp = Cat(io.i_inst(31 downto 25), io.i_inst(11 downto 7))
            inst_type_bundle.imm12 := U(Cat(b_type_imm_tmp(11), Cat(b_type_imm_tmp(9 downto 4), Cat(b_type_imm_tmp(3 downto 0), b_type_imm_tmp(10)))))
            inst_type_bundle.imm20 := U(0)

            when(funct3 === U"000") { // BEQ
                inst_type_bundle.inst_type := U(5)
            } .elsewhen(funct3 === U"001") { // BNE
                inst_type_bundle.inst_type := U(6)
            } .elsewhen(funct3 === U"100") { // BLT
                inst_type_bundle.inst_type := U(7)
            } .elsewhen(funct3 === U"101") { // BGE
                inst_type_bundle.inst_type := U(8)
            } .elsewhen(funct3 === U"110") { // BLTU
                inst_type_bundle.inst_type := U(9)
            } .elsewhen(funct3 === U"111") { // BGEU
                inst_type_bundle.inst_type := U(10)
            } .otherwise { // Illegal
                inst_type_bundle.inst_type := U(0)
            }
        }

        is(U"1100111") {
            // JALR Jump Instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U"100"
            inst_type_bundle.imm12 := io.i_inst(31 downto 20)
            inst_type_bundle.imm20 := U(0)
            inst_type_bundle.inst_type := U(4)
        }

        is(U"1101111") {
            // JAL Jump Instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := U(0)
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U"111" // J_type
            inst_type_bundle.imm12 := U(0)
            val j_type_imm_tmp = io.i_inst(31 downto 12)
            inst_type_bundle.imm20 := U(Cat(j_type_imm_tmp(19), Cat(j_type_imm_tmp(9 downto 0), Cat(j_type_imm_tmp(10), j_type_imm_tmp(18 downto 11)))))
            inst_type_bundle.inst_type := U(3)
        }

        is(U"0010111") {
            // AUIPC Jump Instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := U(0)
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U"111"
            inst_type_bundle.imm12 := U(0)
            inst_type_bundle.imm20 := io.i_inst(31 downto 12)
            inst_type_bundle.inst_type := U(2)
        }

        is(U"0110111") {
            // LUI Load Instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := U(0)
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U"111"
            inst_type_bundle.imm12 := U(0)
            inst_type_bundle.imm20 := io.i_inst(31 downto 12)
            inst_type_bundle.inst_type := U(1)
        }

        is(U"1110011") {
            // Enviroment Instructions
            // normal instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15) // as uimm in CSRRWI/CSRRSI/CSRRCI
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U"100"
            inst_type_bundle.imm12 := io.i_inst(31 downto 20) // as csr
            inst_type_bundle.imm20 := U(0)
            inst_type_bundle.inst_type := U(0)

            // special ECALL/EBRAK instruction
            when(io.i_inst === U"00000000000000000000000001110011") { // ECALL
                inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"110"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(40) 
            } .elsewhen(io.i_inst === U"00000000000100000000000001110011") { // EBREAK
                inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"110"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(41)
            } .elsewhen(funct3 === U"001") { // CSRRW
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15) // as uimm in CSRRWI/CSRRSI/CSRRCI
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20) // as csr
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(42)
            } .elsewhen(funct3 === U"010") { // CSRRS
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15) // as uimm in CSRRWI/CSRRSI/CSRRCI
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20) // as csr
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(43)
            } .elsewhen(funct3 === U"011") { // CSRRC
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15) // as uimm in CSRRWI/CSRRSI/CSRRCI
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20) // as csr
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(44)
            } .elsewhen(funct3 === U"101") { // CSRRWI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15) // as uimm in CSRRWI/CSRRSI/CSRRCI
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20) // as csr
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(45)
            } .elsewhen(funct3 === U"110") { // CSRRSI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15) // as uimm in CSRRWI/CSRRSI/CSRRCI
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20) // as csr
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(46)
            } .elsewhen(funct3 === U"111") { // CSRRCI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15) // as uimm in CSRRWI/CSRRSI/CSRRCI
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20) // as csr
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(47)
            } .otherwise { // Illegal
                inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U(0)
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(0)
            }
        }

        is(U"0011011") {
            // RV64I Immediate Arithmetic Instructions
            // normal instrctions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt in SLLIW/SRLIW/SRAIW
            inst_type_bundle.output_flag := U"001"
            inst_type_bundle.imm12 := U(0)
            inst_type_bundle.imm20 := U(0)
            inst_type_bundle.inst_type := U(0)

            // special ADDIW instruction
            when(funct3 === U"000") { // ADDIW(RV64I)
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(191)
            } .elsewhen(funct3 === U"001" && funct7 === U"0000000") { // SLLIW(RV64I) 
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt in SLLIW/SRLIW/SRAIW
                inst_type_bundle.output_flag := U"001"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(251)
            } .elsewhen(funct3 === U"101" && funct7 === U"0000000") { // SRLIW(RV64I)
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt in SLLIW/SRLIW/SRAIW
                inst_type_bundle.output_flag := U"001"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(261)
            } .elsewhen(funct3 === U"101" && funct7 === U"0100000") { // SRAIW(RV64I)
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt in SLLIW/SRLIW/SRAIW
                inst_type_bundle.output_flag := U"001"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(271)
            }.otherwise { // Illegal
                inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U(0)
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(0)
            }
        }

        is(U"0111011") {
            // RV64I Arithmetic Instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt in SLLIW/SRLIW/SRAIW
            inst_type_bundle.output_flag := U"001"
            inst_type_bundle.imm12 := U(0)
            inst_type_bundle.imm20 := U(0)

            when(funct3 === U"000") {
                when(funct7 === U"0000000") { // ADDW
                    inst_type_bundle.inst_type := U(281)
                } .elsewhen(funct7 === U"0100000") { // SUBW
                    inst_type_bundle.inst_type := U(291)
                } .otherwise { // Illegal
                    inst_type_bundle.inst_type := U(0)
                }
            } .elsewhen(funct3 === U"001") {
                when(funct7 === U"0000000") { // SLLW
                    inst_type_bundle.inst_type := U(301)
                } .otherwise { // Illegal
                    inst_type_bundle.inst_type := U(0)
                }
            } .elsewhen(funct3 === U"101") {
                when(funct7 === U"0000000") { // SRLW
                    inst_type_bundle.inst_type := U(341)
                } .elsewhen(funct7 === U"0100000") { // SRAW
                    inst_type_bundle.inst_type := U(351)
                } .otherwise { // Illegal
                    inst_type_bundle.inst_type := U(0)
                }
            } .otherwise { // Illegal
                inst_type_bundle.inst_type := U(0)
            }
        }

        is(U"0001111") {
            // Memory Model Instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U"100"
            inst_type_bundle.imm12 := io.i_inst(31 downto 20) // 4-bits fm + 4-bits pred + 4-bits succ in FENCE
            inst_type_bundle.imm20 := U(0)

            when(funct3 === U"000") { // FENCE
                inst_type_bundle.inst_type := U(38)
            } .elsewhen(funct3 === U"001") { // FENCE.I
                inst_type_bundle.inst_type := U(39)
            } .otherwise { // Illegal
                inst_type_bundle.inst_type := U(0)
            }
        }

        default { // Illegal
            inst_type_bundle.rd := U(0)
            inst_type_bundle.rs1 := U(0)
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U(0)
            inst_type_bundle.imm12 := U(0)
            inst_type_bundle.imm20 := U(0)
            inst_type_bundle.inst_type := U(0)
        }
    }

    // splice Bundle data and Output
    // check table
    // flag Bundle information -------|
    // |- 001 - R_type.
    // |- 100 - I_type. 
    // |- 011 - S_type. 
    // |- 010 - B_type. 
    // |- 111 - U_type. 
    // |- 111 - J_type. 
    // |- 110 - ECALL/EBRAK
    // Bundle information ------------|
    // |- inst_type --- UInt(16 bits)
    // |- output_flag - UInt(3 bits)
    // |- rs1 --------- UInt(5 bits)
    // |- rs2 --------- UInt(5 bits)
    // |- rd ---------- UInt(5 bits)
    // |- imm12 ------- UInt(12 bits)
    // |- imm20 ------- UInt(20 bits)

    io.o_inst_type := inst_type_bundle.inst_type // instruction type

    when(inst_type_bundle.output_flag === U"001") {
        // R_type
        io.o_rs1 := inst_type_bundle.rs1
        io.o_rs2 := inst_type_bundle.rs2
        io.o_rd := inst_type_bundle.rd
        // not use
        io.o_imm12 := U(0)
        io.o_imm20 := U(0)
    } 
    .elsewhen(inst_type_bundle.output_flag === U"100") {
        // I_type
        io.o_rs1 := inst_type_bundle.rs1
        io.o_rd := inst_type_bundle.rd
        io.o_imm12 := inst_type_bundle.imm12
        // not use
        io.o_rs2 := U(0)
        io.o_imm20 := U(0)
    } 
    .elsewhen(inst_type_bundle.output_flag === U"011") {
        // S_type
        io.o_rs1 := inst_type_bundle.rs1
        io.o_rs2 := inst_type_bundle.rs2
        io.o_imm12 := inst_type_bundle.imm12
        // not use
        io.o_rd := U(0)
        io.o_imm20 := U(0)
    } 
    .elsewhen(inst_type_bundle.output_flag === U"010") {
        // B_type
        io.o_rs1 := inst_type_bundle.rs1
        io.o_rs2 := inst_type_bundle.rs2
        io.o_imm12 := inst_type_bundle.imm12
        // not use
        io.o_rd := U(0)
        io.o_imm20 := U(0)
    } 
    .elsewhen(inst_type_bundle.output_flag === U"111") {
        // J_type
        io.o_rd := inst_type_bundle.rd
        io.o_imm20 := inst_type_bundle.imm20
        // not use
        io.o_rs1 := U(0)
        io.o_rs2 := U(0)
        io.o_imm12 := U(0)
    } .elsewhen(inst_type_bundle.output_flag === U"110") {
        // ECALL/EBREAK
        // not use
        io.o_rs1 := U(0)
        io.o_rs2 := U(0)
        io.o_rd := U(0)
        io.o_imm12 := U(0)
        io.o_imm20 := U(0)
    } .otherwise {
        //Illegal
        io.o_rs1 := U(0)
        io.o_rs2 := U(0)
        io.o_rd := U(0)
        io.o_imm12 := U(0)
        io.o_imm20 := U(0)
    }
}

object decoder {
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
        SimConfig.withWave.doSim(new decoder) { dut =>
            // don't need timer
            // Sim
            println("====================================================")
            println("========== RV64I Instruction Decoder Sim  ==========")
            println("========== 2020/04/16 Build by Tealer.Guo ==========")
            println("====================================================")

            
            // Instructions Sim Template ----------------------------------|
            // println("[Decoder Sim] .")
            // dut.io.i_inst #= binaryToDecWithOutRecur("") //
            // sleep(1)
            // assert(dut.io.o_inst_type.toInt == )
            // assert(dut.io.o_imm20.toInt == binaryToDecWithOutRecur(""))
            // assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur(""))
            // assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur(""))
            // assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur(""))
            // assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur(""))
            // println("[Decoder Sim] SUCCESS.")

            // Instructions Sim
            // Load Instruction
            println("[Decoder Sim] LUI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("00100011111100110010_00010_0110111") // LUI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 1)
            assert(dut.io.o_imm20.toInt == binaryToDecWithOutRecur("00100011111100110010"))
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("00010"))
            println("[Decoder Sim] SUCCESS.")

            // Jump Instructuons
            println("[Decoder Sim] AUIPC.")
            dut.io.i_inst #= binaryToDecWithOutRecur("00011111100000011110_01000_0010111") // AUIPC
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 2)
            assert(dut.io.o_imm20.toInt == binaryToDecWithOutRecur("00011111100000011110"))
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("01000"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] JAL.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0_01111100_0_0101010010_10001_1101111") // JAL
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 3)
            assert(dut.io.o_imm20.toInt == binaryToDecWithOutRecur("0_0101010010_0_01111100"))
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10001"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] JALR.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000011110010_00010_000_11001_1100111") // JALR
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 4)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00010"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000011110010"))
            println("[Decoder Sim] SUCCESS .")

            // Branch Instructions
            println("[Decoder Sim] BEQ.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0_0_11110_00111_11000_000_1_1110_1100011") // BEQ
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 5)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11000"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("00111"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0_11110_1_1110_0"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] BNE.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0_0_11110_00111_11000_001_1_1110_1100011") // BNE
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 6)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11000"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("00111"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0_11110_1_1110_0"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] BLT.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0_0_11110_00111_11000_100_1_1110_1100011") // BLT
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 7)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11000"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("00111"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0_11110_1_1110_0"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] BGE.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0_0_11110_00111_11000_101_1_1110_1100011") // BGE
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 8)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11000"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("00111"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0_11110_1_1110_0"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] BLTU.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0_0_11110_00111_11000_110_1_1110_1100011") // BLTU
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 9)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11000"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("00111"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0_11110_1_1110_0"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] BGEU.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0_0_11110_00111_11000_111_1_1110_1100011") // BGEU
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 10)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11000"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("00111"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0_11110_1_1110_0"))
            println("[Decoder Sim] SUCCESS.")

            // Load Instructions
            println("[Decoder Sim] LB.")
            dut.io.i_inst #= binaryToDecWithOutRecur("00011100110_00011_000_11110_0000011") // LB
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 11)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("00011100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] LH.")
            dut.io.i_inst #= binaryToDecWithOutRecur("00011100110_00011_001_11110_0000011") // LH
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 12)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("00011100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] LW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("010011100110_00011_010_11110_0000011") // LW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 13)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("010011100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] LBU.")
            dut.io.i_inst #= binaryToDecWithOutRecur("00011100110_00011_100_11110_0000011") // LBU
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 14)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("00011100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] LHU.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111111001_00011_101_11110_0000011") // LHU
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 15)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111111001"))
            println("[Decoder Sim] SUCCESS.")

            // RV64I Load Instruction
            println("[Decoder Sim] LD.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111111001_00011_011_11110_0000011") // LD
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 111)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111111001"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] LWU.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111111001_00011_110_11110_0000011") // LWU
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 110)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111111001"))
            println("[Decoder Sim] SUCCESS.")

            // Store Instructions
            println("[Decoder Sim] SB.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0011001_11001_11100_000_00010_0100011") // SB
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 16)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11100"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0011001_00010"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SH.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0011001_11001_11100_001_00010_0100011") // SH
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 17)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11100"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0011001_00010"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0011001_11001_11100_010_00010_0100011") // SW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 18)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11100"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0011001_00010"))
            println("[Decoder Sim] SUCCESS.")

            // RV64I Store Instructions
            println("[Decoder Sim] SD.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0011001_11001_11100_011_00010_0100011") // SD
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 112)
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11100"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0011001_00010"))
            println("[Decoder Sim] SUCCESS.")

            // Immediate Arithmetic Instructions
            println("[Decoder Sim] ADDI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100110_11101_000_11110_0010011") // ADDI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 19)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11101"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLTI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100110_11101_010_11110_0010011") // SLTI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 20)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11101"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLTIU.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100110_11101_011_11110_0010011") // SLTIU
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 21)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11101"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] XORI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100110_11101_100_11110_0010011") // XORI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 22)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11101"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] ORI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100110_11101_110_11110_0010011") // ORI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 23)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11101"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] ANDI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100110_11101_111_11110_0010011") // ANDI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 24)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11101"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLLI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11001_00011_001_11110_0010011") // SLLI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 25)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001")) // as shamt
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLLI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11001_00011_101_11110_0010011") // SRLI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 26)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001")) // as shamt
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLLI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0100000_11001_00011_101_11110_0010011") // SRAI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 27)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001")) // as shamt
            println("[Decoder Sim] SUCCESS.")

            // RV64I Immediate Arithmetic Instructions
            println("[Decoder Sim] ADDIW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100110_11101_000_11110_0011011") // ADDIW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 191)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11101"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100110"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLLIW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11001_00011_001_11110_0011011") // SLLIW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 251)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001")) // as shamt
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SRLIW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11001_00011_101_11110_0011011") // SRLIW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 261)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001")) // as shamt
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SRAIW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0100000_11001_00011_101_11110_0011011") // SRAIW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 271)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("11110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11001")) // as shamt
            println("[Decoder Sim] SUCCESS.")


            // Arithmetic Instructions
            println("[Decoder Sim] ADD.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_000_10110_0110011") // ADD
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 28)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SUB.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0100000_11100_00001_000_10110_0110011") // SUB
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 29)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLL.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_001_10110_0110011") // SLL
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 30)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLT.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_010_10110_0110011") // SLT
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 31)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLTU.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_011_10110_0110011") // SLTU
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 32)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] XOR.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_100_10110_0110011") // XOR
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 33)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SRL.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_101_10110_0110011") // SRL
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 34)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SRA.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0100000_11100_00001_101_10110_0110011") // SRA
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 35)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] OR.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_110_10110_0110011") // OR
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 36)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] AND.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_111_10110_0110011") // AND
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 37)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            // RV64I Arithmetic Instructions
            println("[Decoder Sim] ADDW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_000_10110_0111011") // ADDW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 281)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SUBW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0100000_11100_00001_000_10110_0111011") // SUBW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 291)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SLLW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_001_10110_0111011") // SLLW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 301)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SRLW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0000000_11100_00001_101_10110_0111011") // SRLW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 341)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] SRAW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0100000_11100_00001_101_10110_0111011") // SRAW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 351)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("10110"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs2.toInt == binaryToDecWithOutRecur("11100"))
            println("[Decoder Sim] SUCCESS.")

            // Memory Model Instructions
            println("[Decoder Sim] FENCE.")
            dut.io.i_inst #= binaryToDecWithOutRecur("0010_0011_0001_00011_000_00001_0001111") // FENCE
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 38)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("0010_0011_0001"))
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] FENCE.I.")
            dut.io.i_inst #= binaryToDecWithOutRecur("001000110001_00011_001_00001_0001111") // FENCE.I
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 39)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("00001"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("001000110001"))
            println("[Decoder Sim] SUCCESS.")

            // Enviroment Instructions
            println("[Decoder Sim] ECALL.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000000000000_00000_000_00000_1110011") // ECALL
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 40)
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] EBREAK.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000000000001_00000_000_00000_1110011") // EBREAK
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 41)
            println("[Decoder Sim] SUCCESS.")

            // CSR Load / Store Instructions
            println("[Decoder Sim] CSRRW.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100011_11001_001_00011_1110011") // CSRRW
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 42)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100011")) // csr
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] CSRRS.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100011_11001_010_00011_1110011") // CSRRS
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 43)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100011")) // csr
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] CSRRC.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100011_11001_011_00011_1110011") // CSRRC
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 44)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100011")) // csr
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] CSRRWI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100011_11001_101_00011_1110011") // CSRRWI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 45)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11001")) // uimm
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100011")) // csr
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] CSRRSI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100011_11001_110_00011_1110011") // CSRRSI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 46)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100011")) // csr
            println("[Decoder Sim] SUCCESS.")

            println("[Decoder Sim] CSRRCI.")
            dut.io.i_inst #= binaryToDecWithOutRecur("000111100011_11001_111_00011_1110011") // CSRRCI
            sleep(1)
            assert(dut.io.o_inst_type.toInt == 47)
            assert(dut.io.o_rd.toInt == binaryToDecWithOutRecur("00011"))
            assert(dut.io.o_rs1.toInt == binaryToDecWithOutRecur("11001"))
            assert(dut.io.o_imm12.toInt == binaryToDecWithOutRecur("000111100011")) // csr
            println("[Decoder Sim] SUCCESS.")
            println("=====================================================")
            println("========== ALL Instructions Check Finish.  ==========")
            println("========== 59 Instructions / 59 SUCCESSED. ==========")
            println("=====================================================")
        }
    }
}