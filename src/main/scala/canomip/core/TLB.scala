/**
 * Canomip RISC-V TLB Module
 * 2020/06/11 - First commit - build finish TLB module not test - Tealer.Guo
 * 2020/06/19 - Finish sim and sim success - Tealer.Guo
 */
package canomip.core

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class TLB(tlb_cap: Int, tlb_cap_tag: Int, tlb_mode: Int, len: Int, VA: Int, PA: Int, sim_mode: Boolean) extends Component { 
    // tlb_cap : TLB capacity
    // tlb_cap_tag : TLB capacity tag size
    // tlb_mode : 0 - commom tlb
    // len : page table size(32 bits use SV32 | 64 bits use SV39)
    val io = new Bundle {
    // Ctrl
        // val i_tlb_en = in Bool() // TLB EN
        val i_tlb_addr = in UInt(PA bits) // tlb address
        val o_tlb_statue = out Bool() // True - TLB hit, False - TLB miss

    // TLB read
        val i_tlb_read_en = in Bool() // TLB access EN
        val o_tlb_nread_data = out SInt(len bits) // if TLB hit, output page table data. if TLB miss output S(0)

    // TLB write
        val i_tlb_write_en = in Bool() // TLB write EN
        val i_tlb_nwrite_data = in SInt(len bits) // page table entry
    }

    // Logic

    if(tlb_mode == 0) {
        // commom TLB
        
        if(sim_mode == true) {
            // use sim mode

            // data struct
            case class PageTableEntry() extends Bundle {
                // PTE
                val page_table_entry = SInt(len bits)
                val tlb_tag = UInt(tlb_cap_tag bits) // tag
                val use_flag = Bool() // use flag
            }

            val tlb = Mem(PageTableEntry(), wordCount = tlb_cap) // TLB

            val tlb_addr = io.i_tlb_addr(PA - tlb_cap_tag - 1 downto 0) // tlb addr
            val tlb_tag = io.i_tlb_addr(PA - 1 downto PA - tlb_cap_tag) // tlb tag

            when(io.i_tlb_read_en && !io.i_tlb_write_en) {
                // assess to TLB
            
                val tlb_readed_data = tlb(tlb_addr) // read TLB

                when(tlb_readed_data.tlb_tag === tlb_tag && tlb_readed_data.use_flag === True) {
                    io.o_tlb_statue := True // TLB hit
                    io.o_tlb_nread_data := tlb_readed_data.page_table_entry // output page table entry
                } .otherwise {
                    io.o_tlb_statue := False // TLB miss
                    io.o_tlb_nread_data := S(0) // no data output
                }
            } .elsewhen(io.i_tlb_write_en && !io.i_tlb_read_en) {
                // write TLB

                val tlb_readed_data = tlb(tlb_addr) // read TLB

                // build write data
                val tlb_nwrite_data = new PageTableEntry()
                tlb_nwrite_data.page_table_entry := io.i_tlb_nwrite_data
                tlb_nwrite_data.tlb_tag := tlb_tag
                tlb_nwrite_data.use_flag := True

                tlb(tlb_addr) := tlb_nwrite_data // write TLB

                // not use
                io.o_tlb_statue := False
                io.o_tlb_nread_data := S(0)
            } .otherwise {
                // Illegal
                io.o_tlb_statue := False
                io.o_tlb_nread_data := S(0)
            }
        } else if(sim_mode == true) {
            // use fpga blackBox
            // TODO
        }
    }
}

object TLB {
    def main(args: Array[String]) {
        // 8KB, 46-bits tag | 10-bits addr, commom-cache, 64-bits data, 39-bits VA, 56-bits PA
        SimConfig.withWave.compile(new TLB(1024, 46, 0, 64, 39, 56, true)).doSim { dut =>
            dut.clockDomain.forkStimulus(period = 10) // time period

            // write tlb
            dut.io.i_tlb_addr #= 60
            dut.io.i_tlb_write_en #= true
            dut.io.i_tlb_read_en #= false
            dut.io.i_tlb_nwrite_data #= 666

            dut.clockDomain.waitSampling(1) // time wait

            // read tlb hit
            dut.io.i_tlb_addr #= 60
            dut.io.i_tlb_write_en #= false
            dut.io.i_tlb_read_en #= true
            
            dut.clockDomain.waitSampling(1) // time wait

            assert(dut.io.o_tlb_statue.toBoolean == true)
            assert(dut.io.o_tlb_nread_data.toLong == 666)

            // read tlb miss
            dut.io.i_tlb_addr #= 90
            dut.io.i_tlb_write_en #= false
            dut.io.i_tlb_read_en #= true
            
            dut.clockDomain.waitSampling(1) // time wait

            assert(dut.io.o_tlb_statue.toBoolean == false)
        }
    }
}