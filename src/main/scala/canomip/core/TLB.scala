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
        val i_tlb_addr = in UInt(VA bits) // VA
        val o_tlb_statue = out Bool() // True - TLB hit, False - TLB miss
        val o_tlb_pt_data = out SInt(len bits) // if TLB hit, output page table data. if TLB miss output S(0)
    // TLB write
        val i_tlb_write_en = in Bool() // TLB write EN
        val i_tlb_write_addr = in UInt(VA bits) // write address
        val i_tlb_nwrite_data = in SInt(len bits) // page table entry
    }

    // Logic

    //////////////////////////////////// reverse /////////////////////////////////
    // LRU timer
    // val lru_timer = Reg(UInt(16 bits)) init(0)
    // val inverse_order = Reg(Bool()) init(false)
    // when(lru_timer =/= U"1111111111111111") {
    //     lru_timer := lru_timer + U(1)
    //     inverse_order := False
    // } .otherwise {
    //     inverse_order := True
    // }
    /////////////////////////////////////////////////////////////////////////////

    if(tlb_mode == 0) {
        // commom TLB
        //////////////////////////////////////// reverse ////////////////////////////////////////////////
        // // TLB data structure
        // case class TLBDataStructSV32() extends Bundle {
        //     // SV32 page table
        //     // |31 - 0| ppn1 + ppn0 + rsw + d + a + g + u + x + w + r + v
        //     val v = UInt(1 bits) // 有效位
        //     val r = UInt(1 bits) // 权限位 是否可读
        //     val w = UInt(1 bits) // 权限位 是否可写
        //     val x = UInt(1 bits) // 权限位 是否可执行
        //     val u = UInt(1 bits) // 是否是用户页面
        //     val g = UInt(1 bits) // 指定全局映射
        //     val a = UInt(1 bits) // 已访问位
        //     val d = UInt(1 bits) // 脏位
        //     val rsw = UInt(2 bits) // 操作系统保留
        //     val ppn0 = UInt(10 bits) // 物理页号
        //     val ppn1 = UInt(12 bits) // 物理页号
        // }
        // case class TLBDataStructSV39() extends Bundle {
        //     // SV39 page table
        //     // |64 - 0| reserved + ppn2 + ppn1 + ppn0 + rsw + d + a + g + u + x + w + r + v
        //     val v = UInt(1 bits)
        //     val r = UInt(1 bits)
        //     val w = UInt(1 bits)
        //     val x = UInt(1 bits)
        //     val u = UInt(1 bits)
        //     val g = UInt(1 bits)
        //     val a = UInt(1 bits)
        //     val d = UInt(1 bits)
        //     val rsw = UInt(2 bits)
        //     val ppn0 = UInt(9 bits)
        //     val ppn1 = UInt(9 bits)
        //     val ppn2 = UInt(26 bits)
        //     val reserved = UInt(10 bits)
        // }
        ///////////////////////////////////////////////////////////////////////////////////////
        case class PageTableEntry() extends Bundle {
            // PTE
            val page_table_entry = SInt(len bits)
            val tlb_tag = UInt(tlb_cap_tag bits) // tag
            val use_flag = Bool() // use flag
            // val last_use_time = UInt(16 bits) // timer value
        }
        if(sim_mode == true) {
            // use sim mode
            val tlb = Mem(PageTableEntry(), wordCount = tlb_cap) // TLB
            when(io.i_tlb_acc_en && !io.i_tlb_write_en) {
                // assess to TLB
                val tlb_readed_data = tlb(tlb_addr)
                val tlb_addr = io.i_tlb_addr(len - tlb_cap_tag downto tlb_cap_tag) // tlb addr
                val tlb_tag = io.i_tlb_addr(tlb_cap_tag downto 0) // tlb tag
                when(tlb_readed_data.tag === tlb_tag && tlb_readed_data.use_flag === True) {
                    io.o_tlb_statue := True // TLB hit
                    io.o_tlb_pt_data := tlb_readed_data.page_table_entry // output page table entry
                    // LRU reverse
                    // tlb_readed_data.last_use_time := lru_timer // update time
                    // tlb(tlb_addr) := tlb_readed_data // write back TLB
                } .otherwise {
                    io.o_tlb_statue := False // TLB miss
                }
            } .elsewhen(io.i_tlb_write_en && !io.i_tlb_acc_en) {
                // write TLB
                val tlb_addr = io.i_tlb_write_addr(len - tlb_cap_tag downto tlb_cap_tag) // tlb addr
                val tlb_tag = io.i_tlb_write_addr(tlb_cap_tag downto 0) // tlb tag
                val tlb_readed_data = tlb(tlb_addr)
                // build write data
                val tlb_nwrite_data = new PageTableEntry()
                tlb_nwrite_data.page_table_entry := io.i_tlb_nwrite_data
                tlb_nwrite_data.tlb_tag := tlb_tag
                // LRU reverse
                // tlb_nwrite_data.last_use_time := lru_timer
                tlb_nwrite_data.use_flag := True
                // write TLB
                tlb(tlb_addr) := tlb_nwrite_data
            } .otherwise {
                // Illegal
                io.o_tlb_statue := False
                io.o_tlb_pt_data := S(0)
            }
        }
    }
}

object TLB {
    def main(args: Array[String]) {
        // TODO : main func
    }
}