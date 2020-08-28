/**
 * RISC-V CPU Cache with MMU and TLB
 * 2020/7/11 - First build by Tealer.Guo
 */
package canomip.core

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class cahce(physicl_addr: Int, virtual_addr: Int, len: Int) extends Component {
    val io = new Bundle {
    // Cache IO
        // address
        // read
        val i_cache_read_en = in Bool() // read en
        val o_cache_nread_data = out SInt(len bits) // output need read data
        // write
        val i_cache_write_en = in Bool() // write en
        val i_cache_write_data = in SInt(len bits) // input need write data

    // MEM IO
        // address
        val o_mem_addr = out UInt(physicl_addr bits) // memory access address
        // read
        val o_mem_read_en = out Bool() // read en
        val i_mem_nread_data = in SInt(len bits) // memory access need read data
        // write
        val o_mem_write_data = out SInt(len bits) // memory access write data

    // CSR IO
        // read
        val i_csr_read_mstatue = in SInt(len bits) // mstatue
        val i_csr_read_sstatue = in SInt(len bits) // sstatue
        val i_csr_read_ustatue = in SInt(len bits) // ustatue
        val i_csr_read_satp = in SInt(len bits) // S-Mode satp
        // write
        val o_csr_write_en = out Bool() // write en
        val o_csr_write_addr = out UInt(12 bits) // csr write address 12-bits
        val o_csr_write_data = out SInt(len bits) // csr write data

    // Trap-return inst statue
        val i_trapret_statue = in UInt(2 bits) // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET
    }
}