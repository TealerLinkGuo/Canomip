/**
 * RISC-V Data Extend Module
 * First commit by Tealer.Guo
 * 2020/04/18 - Build extend basic file and class - First commit
 */
package canomip

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class signExtend(len: Int) extends Component {
    val io = new Bundle {
        // TODO : io
    }
}

class zeroExtend(len: Int)) extends Component {
    val io = new Bundle {
        // TODO : io
    }
}

object extend {
    def main(args: Array[String]) {
        // Tester
        SimConfig.withWave.doSim(new signExtend(12)) { dut =>
            // TODO : Sim
        }
        SimConfig.withWave.doSim(new zeroExtend(12)) { dut =>
            // TODO : Sim
        }
    }
}