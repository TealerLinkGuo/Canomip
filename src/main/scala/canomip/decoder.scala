/**
 * RISC-V instruction decoder
 * First Commit by Tealer.Guo
 * 2020/04/06 - Build decoder file and basic class - First Commit
 * 2020/04/15 - Build full RV32I/RV64I decoder logic but no putput, no sim - Tealer.Guo
 * 2020/04/16 - Fix some bugs and finish the RV64I decoder, still no sim - Tealer.Guo
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

            // special I_type instruction
            when(funct3 === U"001" && funct7 === U"0000000") { // SLLI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt
                inst_type_bundle.output_flag := U"001"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(25)
            } .elsewhen(funct3 === U"101" && funct7 === U"0000000") { // SRLI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt
                inst_type_bundle.output_flag := U"001"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(26)
            } .elsewhen(funct3 === U"101" && funct7 === U"0100000") { // SRAI
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := io.i_inst(24 downto 20) // as shamt
                inst_type_bundle.output_flag := U"001"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(27)
            } .otherwise {
                inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U(0)
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(0)
            }

            when(funct3 === U"000") { // ADDI
                inst_type_bundle.inst_type := U(19)
            }
            .elsewhen(funct3 === U"010") { // SLTI
                inst_type_bundle.inst_type := U(20)
            }
            .elsewhen(funct3 === U"011") { // SLTIU
                inst_type_bundle.inst_type := U(21)
            }
            .elsewhen(funct3 === U"100") { // XORI
                inst_type_bundle.inst_type := U(22)
            }
            .elsewhen(funct3 === U"110") { // ORI
                inst_type_bundle.inst_type := U(23)
            }
            .elsewhen(funct3 === U"111") { // ANDI
                inst_type_bundle.inst_type := U(24)
            }
            .otherwise { // Illegal
                inst_type_bundle.inst_type := U(0)
            }
        }

        is(U"0100011") {
            // Store Instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := io.i_inst(19 downto 15)
            inst_type_bundle.rs2 := U(0)
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
            }
            .elsewhen(funct3 === U"001") { // BNE
                inst_type_bundle.inst_type := U(6)
            }
            .elsewhen(funct3 === U"100") { // BLT
                inst_type_bundle.inst_type := U(7)
            }
            .elsewhen(funct3 === U"101") { // BGE
                inst_type_bundle.inst_type := U(8)
            }
            .elsewhen(funct3 === U"110") { // BLTU
                inst_type_bundle.inst_type := U(9)
            }
            .elsewhen(funct3 === U"111") { // BGEU
                inst_type_bundle.inst_type := U(10)
            }
            .otherwise { // Illegal
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
            inst_type_bundle.output_flag := U"011"
            inst_type_bundle.imm12 := U(0)
            inst_type_bundle.imm20 := io.i_inst(31 downto 12)
            inst_type_bundle.inst_type := U(2)
        }

        is(U"0110111") {
            // LUI Load Instructions
            inst_type_bundle.rd := io.i_inst(11 downto 7)
            inst_type_bundle.rs1 := U(0)
            inst_type_bundle.rs2 := U(0)
            inst_type_bundle.output_flag := U"011"
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

            // special ECALL/EBRAK instruction
            when(io.i_inst(31 downto 20) === U(1)) { // ECALL
                inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"110"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(40)
            } .elsewhen(io.i_inst(31 downto 20) === U(0)) { // RBREAK
                inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"110"
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(41)
            } .otherwise { // Illegal
                inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U(0)
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(0)
            }

            when(funct3 === U"001") { // CSRRW
                inst_type_bundle.inst_type := U(42)
            }
            .elsewhen(funct3 === U"010") { // CSRRS
                inst_type_bundle.inst_type := U(43)
            }
            .elsewhen(funct3 === U"011") { // CSRRC
                inst_type_bundle.inst_type := U(44)
            }
            .elsewhen(funct3 === U"101") { // CSRRWI
                inst_type_bundle.inst_type := U(45)
            }
            .elsewhen(funct3 === U"110") { // CSRRSI
                inst_type_bundle.inst_type := U(46)
            }
            .elsewhen(funct3 === U"111") { // CSRRCI
                inst_type_bundle.inst_type := U(47)
            }
            .otherwise { // Illegal
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

            // special ADDIW instruction
            when(funct3 === U"000") { // ADDIW(RV64I)
                inst_type_bundle.rd := io.i_inst(11 downto 7)
                inst_type_bundle.rs1 := io.i_inst(19 downto 15)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U"100"
                inst_type_bundle.imm12 := io.i_inst(31 downto 20)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(191)
            } .otherwise { // Illegal
                inst_type_bundle.rd := U(0)
                inst_type_bundle.rs1 := U(0)
                inst_type_bundle.rs2 := U(0)
                inst_type_bundle.output_flag := U(0)
                inst_type_bundle.imm12 := U(0)
                inst_type_bundle.imm20 := U(0)
                inst_type_bundle.inst_type := U(0)
            }

            when(funct3 === U"001") {
                when(funct7 === U"0000000") {  // SLLIW(RV64I)
                    inst_type_bundle.inst_type := U(251)
                } .otherwise {
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .elsewhen(funct3 === U"101") {
                when(funct7 === U"0000000") { // SRLIW(RV64I)
                    inst_type_bundle.inst_type := U(261)
                } .elsewhen(funct7 === U"0100000") { // SRAIW(RV64I)
                    inst_type_bundle.inst_type := U(271)
                } .otherwise { // Illegal
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .otherwise { // Illegal
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
            }
            .elsewhen(funct3 === U"001") {
                when(funct7 === U"0000000") { // SLLW
                    inst_type_bundle.inst_type := U(301)
                } .otherwise { // Illegal
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .elsewhen(funct3 === U"101") {
                when(funct7 === U"0000000") { // SRLW
                    inst_type_bundle.inst_type := U(341)
                } .elsewhen(funct7 === U"0100000") { // SRAW
                    inst_type_bundle.inst_type := U(351)
                } .otherwise { // Illegal
                    inst_type_bundle.inst_type := U(0)
                }
            }
            .otherwise { // Illegal
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
            }
            .elsewhen(funct3 === U"001") { // FENCE.I
                inst_type_bundle.inst_type := U(39)
            }
            .otherwise { // Illegal
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
    // flag Bundle information --------------------------|
    // |----- 001 - R_type.
    // |----- 100 - I_type. 
    // |----- 011 - S_type. 
    // |----- 010 - B_type. 
    // |----- 011 - U_type. 
    // |----- 111 - J_type. 
    // |----- 110 - ECALL/EBRAK
    // Bundle information -------------------------------|
    // |----- inst_type --- UInt(16 bits)
    // |----- output_flag - UInt(3 bits)
    // |----- rs1 --------- UInt(5 bits)
    // |----- rs2 --------- UInt(5 bits)
    // |----- rd ---------- UInt(5 bits)
    // |----- imm12 ------- UInt(12 bits)
    // |----- imm20 ------- UInt(20 bits)

    io.o_inst_type := inst_type_bundle.inst_type // instruction type

    when(inst_type_bundle.output_flag === U"001") {
        // R_type
        io.o_rs1 := inst_type_bundle.rs1
        io.o_rs2 := inst_type_bundle.rs2
        io.o_rd := inst_type_bundle.rd
        // not use
        io.o_imm12 := U(0)
        io.o_imm20 := U(0)
    } .elsewhen(inst_type_bundle.output_flag === U"100") {
        // I_type
        io.o_rs1 := inst_type_bundle.rs1
        io.o_rd := inst_type_bundle.rd
        io.o_imm12 := inst_type_bundle.imm12
        // not use
        io.o_rs2 := U(0)
        io.o_imm20 := U(0)
    } .elsewhen(inst_type_bundle.output_flag === U"011") {
        // S_type
        io.o_rs1 := inst_type_bundle.rs1
        io.o_rs2 := inst_type_bundle.rs2
        io.o_imm12 := inst_type_bundle.imm12
        // not use
        io.o_rd := U(0)
        io.o_imm20 := U(0)
    } .elsewhen(inst_type_bundle.output_flag === U"010") {
        // B_type
        io.o_rs1 := inst_type_bundle.rs1
        io.o_rs2 := inst_type_bundle.rs2
        io.o_imm12 := inst_type_bundle.imm12
        // not use
        io.o_rd := U(0)
        io.o_imm20 := U(0)
    } .elsewhen(inst_type_bundle.output_flag === U"011") {
        // U_type
        io.o_rd := inst_type_bundle.rd
        io.o_imm20 := inst_type_bundle.imm20
        // not use
        io.o_rs1 := U(0)
        io.o_rs2 := U(0)
        io.o_imm12 := U(0)
    } .elsewhen(inst_type_bundle.output_flag === U"111") {
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
        // Tester
        SimConfig.withWave.compile(new decoder).doSim { dut =>
            // timer
            dut.clockDomain.forkStimulus(period = 10) // time period
            // TODO : Sim code
        }
    }
}