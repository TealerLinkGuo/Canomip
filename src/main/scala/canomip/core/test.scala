package canomip.core

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class test extends Component {
    val io = new Bundle {
        val i_a = in UInt(64 bits)
        val o_data = out UInt(64 bits)
    }

    val aa = Reg(UInt(64 bits)) init(0)
    aa := io.i_a
    io.o_data := io.i_a + aa
}

object test {
    def main(args: Array[String]) {
        // Tester
        SimConfig.withWave.compile(new test).doSim { dut =>
            dut.clockDomain.forkStimulus(period = 10) // time period

            dut.io.i_a #= 1
            dut.clockDomain.waitSampling(1)

            dut.io.i_a #= 3
            dut.clockDomain.waitSampling(1)

            dut.io.i_a #= 6
            dut.clockDomain.waitSampling(1)

            // dut.clockDomain.waitSampling(5)
        }
    }
}

class test2 extends Component {
    val io = new Bundle {
        val i_a = in UInt(64 bits)
        val o_b = out UInt(64 bits)
        val o_b2 = out UInt(64 bits)
    }

    io.o_b := U(6667)
    io.o_b2 := io.i_a
}

object test2 {
    def main(args: Array[String]) {
        // Tester
        SimConfig.withWave.doSim(new test2) { dut =>

            if(dut.io.o_b.toBigInt == 666) {
                dut.io.i_a #= 1234
            } else {
                dut.io.i_a #= 1
            }

            println(dut.io.o_b.toBigInt)

            sleep(1)
        }
    }
}