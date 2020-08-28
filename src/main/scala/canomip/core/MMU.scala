/**
 * RISC-V CPU Memory manage unit MMU
 * Used for translate virtual address to physical address
 * Second build
 * 2020/8/20 - First build by Tealer.Guo
 */
package canomip.core

import spinal.core._
import spinal.sim._
import spinal.core.sim._

class MMU(fpga_mode: Bool, virtual_addr_len: Int, physical_addr_len: Int, bit_len: Int, cache_tag_len: Int, cache_capacity: Int, cache_addr_len: Int, cache_tag_len: Int) {
    val io = new Bundle {
    // MMU IO
        // virtual address
        val i_virtual_addr = in UInt(virtual_addr_len bits)

        // physical address
        val o_physical_addr = out UInt(physical_addr_len bits)

    // TLB IO
        // Ctrl
        val o_tlb_addr = out UInt(physical_addr_len bits) // tlb address
        val i_tlb_statue = in Bool() // True - TLB hit, False - TLB miss

        // TLB read
        val o_tlb_read_en = in Bool() // TLB access EN
        val i_tlb_nread_data = out SInt(bit_len bits) // if TLB hit, output page table data. if TLB miss output S(0)

        // TLB write
        val o_tlb_write_en = in Bool() // TLB write EN
        val o_tlb_nwrite_data = in SInt(bit_len bits) // page table entry

    // Mem IO
        // Ctrl mem
        val o_mem_addr = out UInt(physical_addr_len bits) // need memory access address

        // read mem
        val o_mem_read_en = out Bool() // read memory EN
        val i_mem_nread_data = in SInt(bit_len bits) // memory need read data

        // write mem
        val o_mem_write_en = out Bool() // write memory EN
        val o_mem_nwrite_data = out SInt(bit_len bits) // memory need write data

    // CSR IO
        // CSR read
        val i_csr_read_mstatue = in SInt(len bits) // mstatue
        val i_csr_read_sstatue = in SInt(len bits) // sstatue
        val i_csr_read_ustatue = in SInt(len bits) // ustatue
        val i_csr_read_satp = in SInt(len bits) // S-Mode satp
        // CSR write
        val o_csr_write_en = out Bool() // write en
        val o_csr_write_addr = out UInt(12 bits) // csr write address 12-bits
        val o_csr_write_data = out SInt(len bits) // csr write data

    // Trap-return instruction statue
        // get privilege level now
        val i_ret_inst_statue = in UInt(2 bits) // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET
    }

    // Logic

    // Write error to CSR ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    def write_error_CSR(error_code: UInt,  privilege_now: UInt): Unit = {

        // utval 0x043, stval 0x143

        when(privilege_now === U"01") {
            // S-Mode

            // write error
            when(error_code === U(3)) {
                // 未对齐
                io.o_csr_write_en := True
                io.o_csr_nwrite_addr := U(323) // stvel
                io.o_csr_nwrite_data := S(4) // Load address misaligned
            } .elsewhen(error_code === U(1) || error_code === U(2) || error_code === U(4) || error_code === U(5)) {
                // 权限错误, MXR error, PTE无效, 特权级错误
                io.o_csr_write_en := True
                io.o_csr_nwrite_addr := U(323) // stvel
                io.o_csr_nwrite_data := S(13) // Load page fault
            } .otherwise {
                // Correct
                io.o_csr_write_en := False
                io.o_csr_nwrite_addr := U(0) // stvel
                io.o_csr_nwrite_data := S(666)
            } .elsewhen(privilege_now === U"00") {
                // U-Mode

                // write error
                when(error_code === U(3)) {
                    // 未对齐
                    io.o_csr_write_en := True
                    io.o_csr_nwrite_addr := U(67) // utvel
                    io.o_csr_nwrite_data := S(4) // Load address misaligned
                } .elsewhen(error_code === U(1) || error_code === U(2) || error_code === U(4) || error_code === U(5)) {
                    // 权限错误, MXR error, PTE无效, 特权级错误
                    io.o_csr_write_en := True
                    io.o_csr_nwrite_addr := U(67) // utvel
                    io.o_csr_nwrite_data := S(13) // Load page fault
                } .otherwise {
                    // Correct
                    io.o_csr_write_en := False
                    io.o_csr_nwrite_addr := U(0) // utvel
                    io.o_csr_nwrite_data := S(666) // Load page fault
                }
            } .otherwise {
                // Illegal
                // M-Mode don't need mtval
                io.o_csr_write_en := False
            }
        }
    }
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // Get current privilege ///////////////////////////////////////////////////////////////////////////////////////////////////////
    def get_current_privilege(mstatue_data: SInt, sstatue_data: SInt, ustatue_data: SInt, ret_inst_statue: UInt): UInt = {
        // Get privilege

        // read csr reg sstatue or ustatue

        val return_value = new Bundle {
            val rv = UInt(2 bits)
        }

        when(ret_inst_statue === U"01") {
            // MRET
            return_value.rv := U(mstatue_data(12 downto 11)) // mstatue MPP[1:0] (current privilege)
        } .elsewhen(ret_inst_statue === U"10") {
            // SRET
            return_value.rv := U(sstatue_data(12 downto 11)) // sstatue MPP[1:0] (current privilege)
        } .elsewhen(ret_inst_statue === U"11") {
            // URET
            return_value.rv := U(ustatue_data(12 downto 11)) // ustatue MPP[1:0] (current privilege)
        } .otherwise {
            // no exec Trap-ret instruction
            return_value.rv := U"11" // default MRET
        }

        return return_value.rv
    }
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // Test PTE is illegal or not /////////////////////////////////////////////////////////////////////////////////////////////////////
     def test_PTE_is_illgal(pte_data: SInt, node_now: UInt, level_index: SInt, mstatue_data: SInt, privilege_now: UInt): UInt = {
        // node_now - 0 : have next node, - 1 : don't have next node // TODO : last node test 20/08/25
        // return value 0 - no error,  retrun value 1 - 权限错误(rwx位于当前模式不符)
        // return value 2 - MXR error, return value 3 - 未对齐
        // return value 4 - 特权级错误, return value 5 - PTE无效
        // level_index - level number

        // test PTE data is illegal

        val return_value = new Bundle {
            val rv = UInt(3 bits)
        }

        // get SUM and MXR
        val MXR = mstatue_data(19)
        val SUM = mstatue_data(18) // TODO : SUM value check error

        when(level_index === S(0)) {
            // don't have next node
            when(U(pte_data(0)) === U"1") {
                // PTE有效
                when(U(pte_data(1)) === U"1" && privilege_now === U"01") {
                    when(privilege_now === U"01" && U(pte_data(4)) === U"0") {
                        // S-Mode
                        when(U(MXR) === U"1") {
                            // only success in pte.r = 1
                            when(U(pte_data(1)) === U"1" && U(pte_data(2)) =/= U"1" || U(pte_data(3)) =/= U"1") {
                                return_value.rv := U(0) // correct
                            } .otherwise {
                                return_value.rv := U(2) // MXR error
                            }
                        } .otherwise {
                            // success with pte.r = 1 && pte.x = 1
                            when(U(pte_data(1)) === U"1" && U(pte_data(3)) === U"1") {
                                return_value.rv := U(0) // correct
                            } .otherwise {
                                return_value.rv := U(2) // MXR error
                            }
                        }
                    } .otherwise {
                        return_value.rv := U(4) // 特权级错误
                    }
                } .elsewhen(privilege_now === U"00" && U(pte_data(4)) === U"1") {
                    // U-Mode
                    when(U(MXR) === U"1") {
                            // only success in pte.r = 1
                            when(U(pte_data(1)) === U"1" && U(pte_data(2)) =/= U"1" || U(pte_data(3)) =/= U"1") {
                                return_value.rv := U(0) // correct
                            } .otherwise {
                                return_value.rv := U(2) // MXR error
                            }
                        } .otherwise {
                            // success with pte.r = 1 && pte.x = 1
                            when(U(pte_data(1)) === U"1" && U(pte_data(3)) === U"1") {
                                return_value.rv := U(0) // correct
                            } .otherwise {
                                return_value.rv := U(2) // MXR error
                            }
                        }
                } .elsewhen(U(pte_data(6)) === U"0") {
                    // pte.a = 0
                    return_value.rv := U(5) // debug change to 3, it is 5
                }.otherwise {
                    return_value.rv := U(1) // 权限错误(rwx位于当前模式不符)
                }
            } .otherwise {
                return_value.rv := U(5) // PTE无效 // debug change to 0, it is 5
            }
        } .elsewhen(level_index > S(0)) { // level_index > 0
            // have next node
            when(U(pte_data(0)) === U"1") {
                when(U(pte_data(1)) === U"0" && U(pte_data(2)) === U"1") {
                    return_value.rv := U(1) // 权限错误(rwx位于当前模式不符)
                } .otherwise {
                    return_value.rv := U(0) // correct
                }
            } .otherwise {
                return_value.rv := U(5) // PTE无效 // debug change to 0, it is 5
            }
        } .otherwise { // level_index < 0
            // 未对齐
            return_value.rv := U(3)
        }

        return return_value.rv
    }
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // MMU Logic /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    if(fpga_mode == false) {
        // Commom sim mode

        // data bundle
        case class cacheDataStruct() extends Bundle {
            val data = SInt(bit_len bits) // cache data
            val tag = UInt(cache_tag_len bits) // cache tag
            val use_flag = Bool()
        }

        // search PTE from TLB, cahce and memory ///////////////////////////////////////////////////////////////////////////////////////
        def search_PTE_commom(cache: Mem[cacheDataStruct], addr: UInt): SInt = {

            // set return value
            val return_value = new Bundle {
                val rv = SInt(bit_len bits)
            }

            // search PTE from TLB
            // enable TLB
            io.o_tlb_addr := addr
            io.o_tlb_write_en := True // enable read
            io.o_tlb_write_en := False

            when(io.i_tlb_statue === False) {
                // TLB miss

                // search PTE from cache
                val cache_data = cache(addr(cache_addr_len - 1 downto 0))

                when(cache_data.tag =/= addr(physical_addr_len - 1 downto cache_addr_len) || cache_data.use_flag =/= True) {
                    // cache miss

                    // search PTE from memory
                    // WEARNING : Memory data no check, maybe data after access memory is illegal !
                    io.o_mem_read_en := True
                    io.o_mem_write_en := False
                    io.o_mem_addr := addr(physical_addr_len - 1 downto 0)
                    
                    val mem_data = io.i_mem_nread_data // get return data

                    // write TLB
                    io.o_tlb_write_en := False // disable read
                    io.o_tlb_write_en := True // enable write

                    io.o_tlb_nwrite_data := mem_data // write data
                } .otherwise {
                    // cache hit
                    return_value.rv = cache_data.data // return data

                    // write tlb
                    io.o_tlb_write_en := False // disable read
                    io.o_tlb_write_en := True // enable write

                    io.o_tlb_nwrite_data := cache_data.data // write data
                }
            } .otherwise {
                // TLB hit
                return_value.rv = io.i_tlb_nread_data // return data
            }
        }
        //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    }
}