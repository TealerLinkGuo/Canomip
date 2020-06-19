/**
 * Canomip RISC-V TLB Module
 * 2020/06/11 - First commit - build finish TLB module not test - Tealer.Guo
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
    // TLB access
        val i_tlb_acc_en = in Bool() // TLB access EN
        val i_tlb_addr = in UInt(PA bits) // VA
        val o_tlb_statue = out Bool() // True - TLB hit, False - TLB miss
        val o_tlb_pt_data = out SInt(len bits) // if TLB hit, output page table data. if TLB miss output S(0)
    // TLB write
        val i_tlb_write_en = in Bool() // TLB write EN
        val i_tlb_write_addr = in UInt(VA bits) // write address
        val i_tlb_nwrite_data = in SInt(len bits) // page table entry
    }

    // Logic

    if(tlb_mode == 0) {
        // commom TLB
        case class PageTableEntry() extends Bundle {
            // PTE
            val page_table_entry = SInt(len bits)
            val tlb_tag = UInt(tlb_cap_tag bits) // tag
            val use_flag = Bool() // use flag
        }
        if(sim_mode == true) {
            // use sim mode

            val tlb = Mem(PageTableEntry(), wordCount = tlb_cap) // TLB

            when(io.i_tlb_acc_en && !io.i_tlb_write_en) {
                // assess to TLB
                val tlb_addr = io.i_tlb_addr(len - tlb_cap_tag downto tlb_cap_tag) // tlb addr
                val tlb_tag = io.i_tlb_addr(tlb_cap_tag downto 0) // tlb tag

                val tlb_readed_data = tlb(tlb_addr) // read TLB

                when(tlb_readed_data.tlb_tag === tlb_tag && tlb_readed_data.use_flag === True) {
                    io.o_tlb_statue := True // TLB hit
                    io.o_tlb_pt_data := tlb_readed_data.page_table_entry // output page table entry
                } .otherwise {
                    io.o_tlb_statue := False // TLB miss
                }
            } .elsewhen(io.i_tlb_write_en && !io.i_tlb_acc_en) {
                // write TLB
                val tlb_addr = io.i_tlb_write_addr(len - tlb_cap_tag downto tlb_cap_tag) // tlb addr
                val tlb_tag = io.i_tlb_write_addr(tlb_cap_tag downto 0) // tlb tag

                val tlb_readed_data = tlb(tlb_addr) // read TLB

                // build write data
                val tlb_nwrite_data = new PageTableEntry()
                tlb_nwrite_data.page_table_entry := io.i_tlb_nwrite_data
                tlb_nwrite_data.tlb_tag := tlb_tag
                tlb_nwrite_data.use_flag := True

                tlb(tlb_addr) := tlb_nwrite_data // write TLB
            } .otherwise {
                // Illegal
                io.o_tlb_statue := False
                io.o_tlb_pt_data := S(0)
            }
        } else if(sim_mode == true) {
            // use fpga blackBox
            // TODO
        }
    }
}

object TLB {
    def main(args: Array[String]) {
        // TODO : main func
    }
}