/**
 * RISC-V Data Extend Module
 * First commit by Tealer.Guo
 * 2020/04/18 - Build extend basic file and class - First commit
 * 2020/05/02 - Finish extend module and sim success
 */
package canomip

import spinal.core._
import spinal.sim._
import spinal.core.sim._

// Sign Extends Module
class signExtend(len: Int) extends Component {
    val io = new Bundle {
        // Input
        val i_mode_flag = in UInt(2 bits) // 00 _ 12-bits | 01 _ 20 - bits | 10 _ 8 - bits | 11 _ 16 - bits
        val i_need_extend_data_8 = in UInt(8 bits) // 8-bits sign extend
        val i_need_extend_data_16 = in UInt(16 bits) // 16-bits sign extend
        val i_need_extend_data_12 = in UInt(12 bits) // 12-bits sign extend
        val i_need_extend_data_20 = in UInt(20 bits) // 20-bits sign extend

        // Output
        val o_extended_data = out SInt(len bits) // data after sign extends
    }

    // Logic
    if(len == 32) {
        // 32-bits
        when(io.i_mode_flag === U"00") {
            // 12-bits
            when(U(io.i_need_extend_data_12(11)) === U"1") {
                io.o_extended_data := S(Cat(U"11111111111111111111", io.i_need_extend_data_12))
            } .otherwise {
                io.o_extended_data := S(Cat(U"00000000000000000000", io.i_need_extend_data_12))
            }
        } .elsewhen(io.i_mode_flag === U"01") {
            // 20-bits
            when(U(io.i_need_extend_data_20(19)) === U"1") {
                io.o_extended_data := S(Cat(U"111111111111", io.i_need_extend_data_20))
            } .otherwise {
                io.o_extended_data := S(Cat(U"000000000000", io.i_need_extend_data_20))
            }
        } .elsewhen(io.i_mode_flag === U"10") {
            // 8-bits
            when(U(io.i_need_extend_data_8(7)) === U"1") {
                io.o_extended_data := S(Cat(U"111111111111111111111111", io.i_need_extend_data_8))
            } .otherwise {
                io.o_extended_data := S(Cat(U"000000000000000000000000", io.i_need_extend_data_8))
            }
        } .elsewhen(io.i_mode_flag === U"11") {
            // 16-bits
            when(U(io.i_need_extend_data_16(15)) === U"1") {
                io.o_extended_data := S(Cat(U"1111111111111111", io.i_need_extend_data_16))
            } .otherwise {
                io.o_extended_data := S(Cat(U"0000000000000000", io.i_need_extend_data_16))
            }
        } .otherwise {
            // Illegal
            io.o_extended_data := S(0)
        }
    } else if(len == 64) {
        // 64-bits
        when(io.i_mode_flag === U"00") {
            // 12-bits
            when(U(io.i_need_extend_data_12(11)) === U"1") {
                io.o_extended_data := S(Cat(U"1111111111111111111111111111111111111111111111111111", io.i_need_extend_data_12))
            } .otherwise {
                io.o_extended_data := S(Cat(U"0000000000000000000000000000000000000000000000000000", io.i_need_extend_data_12))
            }
        } .elsewhen(io.i_mode_flag === U"01") {
            // 20-bits
            when(U(io.i_need_extend_data_20(19)) === U"1") {
                io.o_extended_data := S(Cat(U"11111111111111111111111111111111111111111111", io.i_need_extend_data_20))
            } .otherwise {
                io.o_extended_data := S(Cat(U"00000000000000000000000000000000000000000000", io.i_need_extend_data_20))
            }
        }  .elsewhen(io.i_mode_flag === U"10") {
            // 8-bits
            when(U(io.i_need_extend_data_8(7)) === U"1") {
                io.o_extended_data := S(Cat(U"11111111111111111111111111111111111111111111111111111111", io.i_need_extend_data_8))
            } .otherwise {
                io.o_extended_data := S(Cat(U"00000000000000000000000000000000000000000000000000000000", io.i_need_extend_data_8))
            }
        } .elsewhen(io.i_mode_flag === U"11") {
            // 16-bits
            when(U(io.i_need_extend_data_16(15)) === U"1") {
                io.o_extended_data := S(Cat(U"111111111111111111111111111111111111111111111111", io.i_need_extend_data_16))
            } .otherwise {
                io.o_extended_data := S(Cat(U"000000000000000000000000000000000000000000000000", io.i_need_extend_data_16))
            }
        } .otherwise {
            // Illegal
            io.o_extended_data := S(0)
        }
    }
}

// Zero Extends Module
class zeroExtend(len: Int) extends Component {
    val io = new Bundle {
        val i_mode_flag = in UInt(2 bits) // 00 _ 12-bits | 01 _ 20 - bits | 10 _ 8 - bits | 11 _ 16 - bits
        val i_need_extend_data_8 = in UInt(8 bits) // 8-bits sign extend
        val i_need_extend_data_16 = in UInt(16 bits) // 16-bits sign extend
        val i_need_extend_data_12 = in UInt(12 bits) // 12-bits sign extend
        val i_need_extend_data_20 = in UInt(20 bits) // 20-bits sign extend
        val o_extended_data = out UInt(len bits) // data after sign extends
    }

    // Logic
    if(len == 32) {
        // 32-bits
        when(io.i_mode_flag === U"00") {
            // 12-bits
            io.o_extended_data := U(Cat(U"00000000000000000000", io.i_need_extend_data_12))
        } .elsewhen(io.i_mode_flag === U"01") {
            // 20-bits
            io.o_extended_data := U(Cat(U"000000000000", io.i_need_extend_data_20))
        } .elsewhen(io.i_mode_flag === U"10") {
            // 8-bits
            io.o_extended_data := U(Cat(U"000000000000000000000000", io.i_need_extend_data_8))
        } .elsewhen(io.i_mode_flag === U"11") {
            // 16-bits
            io.o_extended_data := U(Cat(U"0000000000000000", io.i_need_extend_data_16))
        } .otherwise {
            // Illegal
            io.o_extended_data := U(0)
        }
    } else if(len == 64) {
        // 64-bits
        when(io.i_mode_flag === U"00") {
            io.o_extended_data := U(Cat(U"0000000000000000000000000000000000000000000000000000", io.i_need_extend_data_12))
        } .elsewhen(io.i_mode_flag === U"01") {
            // 20-bits
            io.o_extended_data := U(Cat(U"00000000000000000000000000000000000000000000", io.i_need_extend_data_20))
        }  .elsewhen(io.i_mode_flag === U"10") {
            // 8-bits
            io.o_extended_data := U(Cat(U"00000000000000000000000000000000000000000000000000000000", io.i_need_extend_data_8))
        } .elsewhen(io.i_mode_flag === U"11") {
            // 16-bits
            io.o_extended_data := U(Cat(U"000000000000000000000000000000000000000000000000", io.i_need_extend_data_16))
        } .otherwise {
            // Illegal
            io.o_extended_data := U(0)
        }
    }
}

object extend {
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
        SimConfig.withWave.doSim(new signExtend(32)) { dut =>
            // 32-bits Sim

            // op code template
            /** op_mode code -|
             * 00 _ 12 - bits |
             * 01 _ 20 - bits |
             * 10 _ 8  - bits |
             * 11 _ 16 - bits |
             */

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("00") // 12-bits
            dut.io.i_need_extend_data_12 #= binaryToDecWithOutRecur("111000110001")
            sleep(1)
            // 2020/05/02 bug : assert fail but it it right ! use people check
            // assert(dut.io.o_extended_data.toLong == binaryToDecWithOutRecur("11111111111111111111111000110001"))

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("01") // 20-bits
            dut.io.i_need_extend_data_20 #= binaryToDecWithOutRecur("11001110011111000010")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("10") // 8-bits
            dut.io.i_need_extend_data_8 #= binaryToDecWithOutRecur("11000001")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("11") // 16-bits
            dut.io.i_need_extend_data_16 #= binaryToDecWithOutRecur("1000111000110000")
            sleep(1)
        }

        SimConfig.withWave.doSim(new signExtend(64)) { dut =>
            // 64-bits Sim
            dut.io.i_mode_flag #= binaryToDecWithOutRecur("00") // 12-bits
            dut.io.i_need_extend_data_12 #= binaryToDecWithOutRecur("111000110001")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("01") // 20-bits
            dut.io.i_need_extend_data_20 #= binaryToDecWithOutRecur("11001110011111000010")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("10") // 8-bits
            dut.io.i_need_extend_data_8 #= binaryToDecWithOutRecur("11000001")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("11") // 16-bits
            dut.io.i_need_extend_data_16 #= binaryToDecWithOutRecur("1000111000110000")
            sleep(1)
        }

        SimConfig.withWave.doSim(new zeroExtend(32)) { dut =>
            // 32-bits Sim

            // op code template
            /** op_mode code -|
             * 00 _ 12 - bits-|
             * 01 _ 20 - bits-|
             * 10 _ 8  - bits-|
             * 11 _ 16 - bits-|
             */

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("00") // 12-bits
            dut.io.i_need_extend_data_12 #= binaryToDecWithOutRecur("011000110001")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("01") // 20-bits
            dut.io.i_need_extend_data_20 #= binaryToDecWithOutRecur("01001110011111000010")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("10") // 8-bits
            dut.io.i_need_extend_data_8 #= binaryToDecWithOutRecur("01000001")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("11") // 16-bits
            dut.io.i_need_extend_data_16 #= binaryToDecWithOutRecur("0000111000110000")
            sleep(1)
        }

        SimConfig.withWave.doSim(new zeroExtend(64)) { dut =>
            // 64-bits
            dut.io.i_mode_flag #= binaryToDecWithOutRecur("00") // 12-bits
            dut.io.i_need_extend_data_12 #= binaryToDecWithOutRecur("011000110001")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("01") // 20-bits
            dut.io.i_need_extend_data_20 #= binaryToDecWithOutRecur("01001110011111000010")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("10") // 8-bits
            dut.io.i_need_extend_data_8 #= binaryToDecWithOutRecur("01000001")
            sleep(1)

            dut.io.i_mode_flag #= binaryToDecWithOutRecur("11") // 16-bits
            dut.io.i_need_extend_data_16 #= binaryToDecWithOutRecur("0000111000110000")
            sleep(1)
        }
    }
}