/**
 * MIPS R6 instruction decoder
 * First Ciommit by Tealer.Guo
 * 2020/04/06 - Build decoder file and basic class
 */
package canomip

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class decoder extends Component {
    val io = new Bundle {
        // TODO : io define
    }
    // TODO : logic
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