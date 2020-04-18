/**
 * RISC-V Int ALU
 * First commit by Tealer.Guo
 * 2020/04/18 - Build alu basic file and class - First commit
 */
package canomip

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class alu(len: Int) extends Component {
    val io = new Bundle {
        // Input
        val i_inst_type = in UInt(16 bits)
        val i_rs1_data = in SInt(len bits)
        val i_rs2_imm_data = in SInt(len bits)

        // Output
        val o_res_data = out SInt(len bits)
    }

    // TODO : logic
}

object alu {
    def main(args: Array[String]) {
        // Tester
        SimConfig.withWave.doSim(new alu(64)) { dut =>
            // TODO : Sim
        }
        SimConfig.withWave.doSim(new alu(32)) { dut =>
            // TODO : Sim
        }
    }
}