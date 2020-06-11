/**
 * RISC-V Data Cache Module support risc-v SV39/SV32
 * Page table saved in memory, cache or TLB. when trans VA to PA, need check TLB. TLB miss is handed by hardware.
 * First commit by Tealer.Guo
 * 2020/05/02 - Build extend basic file and class - First commit
 * 2020/06/11 - build finish io bundle and some logic not test - Tealer.Guo
 */
package canomip.core

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class dcache(c_mode: Int, c_cap: Int, c_cap_tag: Int, len: Int, mode: Int, VA: Int, PA: Int, sim_mode: Boolean) extends Component {
        // c_mod 0 - commom cache, c_mod 1 - 4 WAY cache(LRU)
        // c_cap - cache capacity (KB / len bits)
        // c_cap_tag - cache capacity tag size (bits)
        // Mode 0 - SV32 | VA - 32 bits | PA - 34 bits | len - 32 bits
        // Mode 1 - SV39 | VA - 39 bits | PA - 56 bits | len - 64 bits
    val io = new Bundle {
        // Ctrl
    // TLB check
        // access TLB
        val o_tlb_en = out Bool() // TLB EN
        val o_tlb_addr = out UInt(VA bits) // out virtual address to TLB
        val i_tlb_pt_data = in SInt(len bits) // input page table in TLB
    // Cahce RW
        // read cache
        val i_read_en = in Bool() // read EN
        val i_read_addr = in UInt(VA bits) // read data address (Virtual Address)
        val o_nread_data = out SInt(len bits) // need read data
        // write cache
        val i_write_en = in Bool() // write EN
        val i_write_addr = in UInt(VA bits) // write data address (Virtual Address)
        val i_nwrite_data = in SInt(len bits) // need write data
    // Mem access
        // read mem
        val o_mem_read_en = out Bool() // read memory EN
        val o_mem_read_addr = out UInt(PA bits) // read memeory address (maybe VA or PA, extends to PA)
        val i_mem_nread_data = in SInt(len bits) // memory need read data
        // write mem
        val o_mem_write_en = out Bool() // write memory EN
        val o_mem_write_addr = out UInt(PA bits) // write memory address (maybe VA or PA, extends to PA)
        val o_mem_nwrite_data = out SInt(len bits) // memory need write data
    }

    // Logic

    if(c_mod == 0) {
        // commom mode cache
        // data struct
        case class cacheDataStruct() extends Bundle {
            val data = SInt(len bits) // cache data
            val tag = UInt(c_cap_tag bits) // cache tag
            val use_flag = Bool()
        }
        // sim mode
        if(sim_mode == true) {
            // sim mode not use fpga blackBox
            // Logic
            // First use VA check TLB
            val cache_commom = Mem(cacheDataStruct(), wordCount = c_cap) // commom cache
        } else if(sim_mode == false) {
            // use fpga blackBox
        }
    }
}

object dcahce {
    def main(args: Array[String]) {
        // TODO : main func
    }
}