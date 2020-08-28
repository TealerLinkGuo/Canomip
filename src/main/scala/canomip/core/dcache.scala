/**
 * RISC-V Data Cache Module support risc-v SV39/SV32
 * Page table saved in memory, cache or TLB. when trans VA to PA, need check TLB. TLB miss is handed by hardware.
 * write through cache
 * First commit by Tealer.Guo
 * 2020/05/02 - Build extend basic file and class - First commit
 * 2020/06/11 - build finish io bundle and some logic not test - Tealer.Guo
 * 2020/06/13 - build read cache logic not finish not test - Tealer.Guo
 * 2020/06/14 - build finish dcahce no test no sim - Tealer.Guo
 * 2020/06/19 - Fix some bugs, no sim - Tealer.Guo
 * 2020/06/20 - Fix some bugs, now errors can be track, change complex io single - Tealer.Guo
 * 2020/07/02 - Fix bugsn, now can run sim - Tealer.Guo
 */

 /**
  * ERRORS and FUTURE
  * ERROR : Can't compute the correct Page Table address, may be the type convert problem
  * FUTURE : None
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
        val o_tlb_addr = out UInt(PA bits) // need tlb check address
        // read TLB
        val o_tlb_read_en = out Bool() // TLB read EN
        val i_tlb_read_data = in SInt(len bits) // input page table in TLB
        val i_tlb_statue = in Bool() // True - TLB hit, False - TLB miss
        // write TLB
        val o_tlb_write_en = out Bool() // TLB write EN
        val o_tlb_write_data = out SInt(len bits)

    // Cache RW
        // val i_cache_addr = in UInt(VA bits) // need cache rw address
        val i_cache_addr = in UInt(PA bits) // need cache rw address
        // read cache
        val i_read_en = in Bool() // read EN
        val o_nread_data = out SInt(len bits) // need read data
        // write cache
        val i_write_en = in Bool() // write EN
        val i_nwrite_data = in SInt(len bits) // need write data

    // Mem access
        val o_mem_addr = out UInt(PA bits) // need memory access address
        // read mem
        val o_mem_read_en = out Bool() // read memory EN
        val i_mem_nread_data = in SInt(len bits) // memory need read data
        // write mem
        val o_mem_write_en = out Bool() // write memory EN
        val o_mem_nwrite_data = out SInt(len bits) // memory need write data

    // CSR access
        // read csr statue reg and satp reg
        val i_csr_mstatue = in SInt(len bits)
        val i_csr_sstatue = in SInt(len bits)
        val i_csr_ustatue = in SInt(len bits)
        val i_csr_satp = in UInt(len bits)
        // write csr
        val o_csr_write_en = out Bool() // csr write EN
        val o_csr_nwrite_addr = out UInt(12 bits) // csr need write address
        val o_csr_nwrite_data = out SInt(len bits) // csr need write data

    // Trap-return instruction statue
        val i_ret_inst_statue = in UInt(2 bits) // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET

    // Test output
        // val o_level_index = out SInt(3 bits)
        val o_one_level_pte_addr = out UInt(57 bits) // pte addr now
        val o_one_level_pte_satpPPN = out UInt(44 bits) // satp ppn now
        val o_one_level_pte_VPN = out UInt(9 bits) // pte vpn now
        val o_one_level_pte_data = out SInt(64 bits) // pte data now
        val o_cache_onelevepte_tag = out UInt(c_cap_tag bits) // cache tag now
        val o_cache_onelevepte_data = out SInt(64 bits) // cahce data now
        val o_cahce_onelevepte_addr = out UInt(PA - c_cap_tag bits) // cahce addr now
        val o_cache_onelevepte_tag_true = out UInt(c_cap_tag bits) // true cahce tag
    }

    // Logic

    def write_error_to_csr(error_code: UInt, privilege_now: UInt): Unit = {
        // write error to CSR REG

        // utval 0x043. stval 0x143

        // left LATCH
        io.o_csr_write_en := False
        io.o_csr_nwrite_addr := U(0)
        io.o_csr_nwrite_data := S(0)

        // Logic 
        when(privilege_now === U"01") {
            // S-Mode

            // left LATCH
            // io.o_csr_write_en := False
            // io.o_csr_nwrite_addr := U(0)
            // io.o_csr_nwrite_data := S(0)

            // write
            when(error_code === U(3)) {
                // 未对齐
                io.o_csr_write_en := True
                io.o_csr_nwrite_addr := U(323) // stvel
                io.o_csr_nwrite_data := S(4) // Load address misaligned
            // debug change, delete when update
            } .elsewhen(error_code === U(1) || error_code === U(2) || error_code === U(4) || error_code === U(5)) {
                // 权限错误, MXR error, PTE无效, 特权级错误
                io.o_csr_write_en := True
                io.o_csr_nwrite_addr := U(323) // stvel
                io.o_csr_nwrite_data := S(13) // Load page fault
            // } .elsewhen(error_code === U(1)) { // debug used, delete before update
            //     io.o_csr_write_en := True
            //     io.o_csr_nwrite_addr := U(323) // stvel
            //     io.o_csr_nwrite_data := S(14) // Load page fault
            // }.elsewhen(error_code === U(2)) {
            //     io.o_csr_write_en := True
            //     io.o_csr_nwrite_addr := U(323) // stvel
            //     io.o_csr_nwrite_data := S(15) // Load page fault
            // } .elsewhen(error_code === U(4)) {
            //     io.o_csr_write_en := True
            //     io.o_csr_nwrite_addr := U(323) // stvel
            //     io.o_csr_nwrite_data := S(16) // Load page fault
            // } .elsewhen(error_code === U(5)) {
            //     io.o_csr_write_en := True
            //     io.o_csr_nwrite_addr := U(323) // stvel
            //     io.o_csr_nwrite_data := S(17) // Load page fault
            } .otherwise {
                // Correct
                io.o_csr_write_en := False
                io.o_csr_nwrite_addr := U(0) // stvel
                io.o_csr_nwrite_data := S(666)
            }
        } .elsewhen(privilege_now === U"00") {
            // U-Mode

            // left LATCH
            // io.o_csr_write_en := False
            // io.o_csr_nwrite_addr := U(0)
            // io.o_csr_nwrite_data := S(0)

            // write
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
            val carsh = U(0)
            // io.o_csr_write_en := False
            // io.o_csr_nwrite_addr := U(0) // utvel
            // io.o_csr_nwrite_data := S(666) // Load page fault
        }
    }

    def get_current_privilege(mstatue_data: SInt, sstatue_data: SInt, ustatue_data: SInt, ret_inst_statue: UInt): UInt = {
        // Get privilege now
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

    def test_PTE_is_illgal(pte_data: SInt, node_now: UInt, level_index: SInt, mstatue_data: SInt, privilege_now: UInt): UInt = {
        // node_now - 0 : have next node, - 1 : don't have next node
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

    // Logic

    if(c_mode == 0) {
        // commom mode cache

        // data bundle
        case class cacheDataStruct() extends Bundle {
            val data = SInt(len bits) // cache data
            val tag = UInt(c_cap_tag bits) // cache tag
            val use_flag = Bool()
        }

        // Search MEM ////////////////////////////////////////////////////////////////////////////////////////////////////////////

        def search_cache_address_commom_cache(cache: Mem[cacheDataStruct], addr: UInt): SInt = {
            // Search address in cache and mem and return data

            // return data struct
            val return_value = new Bundle {
                val rv = SInt(len bits)
            }

            // First search TLB
            io.o_tlb_read_en := True // enable TLB
            io.o_tlb_addr := addr(55 downto 0)
            // io.o_tlb_addr := addr

            // left LATCH
            io.o_tlb_write_en := False
            io.o_tlb_write_data := S(0)
            io.o_mem_addr := U(0)
            io.o_mem_read_en := False
            io.o_mem_write_en := False
            io.o_mem_nwrite_data := S(0)
            return_value.rv := S(0)
            
            when(io.i_tlb_statue === False) {
                // TLB miss

                val addr_after = addr(55 downto 0) // after split addr
                // val addr_after = addr
                val cache_addr = addr_after(PA - c_cap_tag - 1 downto 0)
                val cache_tag = addr_after(PA - 1 downto PA - c_cap_tag)

                // search cache
                val cache_data = cache(cache_addr)

                // left LATCH
                // io.o_tlb_write_en := False
                // io.o_tlb_write_data := S(0)
                // io.o_mem_addr := U(0)
                // io.o_mem_read_en := False
                // io.o_mem_write_en := False
                // io.o_mem_nwrite_data := S(0)

                when(cache_data.tag === cache_tag && cache_data.use_flag === True) { // cache hit
                    // write TLB
                    io.o_tlb_read_en := False // close TLB read
                    io.o_tlb_write_en := True
                    io.o_tlb_addr := addr(55 downto 0) // after split addr
                    // io.o_tlb_addr := addr
                    io.o_tlb_write_data := cache_data.data

                    // don't use
                    // io.o_mem_addr := U(0)
                    // io.o_mem_read_en := False
                    // io.o_mem_write_en := False
                    // io.o_mem_nwrite_data := S(0)

                    // write return value
                    return_value.rv := cache_data.data
                } .otherwise {
                    // cache miss

                    // mem access
                    io.o_mem_read_en := True
                    io.o_mem_write_en := False
                    io.o_mem_addr := addr(55 downto 0) // after split addr
                    // io.o_mem_addr := addr
                    val mem_data = io.i_mem_nread_data

                    // write cache
                    val nwrite_cache_data = new cacheDataStruct()
                    nwrite_cache_data.data := mem_data
                    nwrite_cache_data.tag := cache_tag
                    nwrite_cache_data.use_flag := True

                    // write
                    cache(cache_addr) := nwrite_cache_data

                    // write TLB
                    io.o_tlb_read_en := False // close TLB read
                    io.o_tlb_write_en := True
                    io.o_tlb_addr := addr(55 downto 0) // after split addr
                    // io.o_tlb_addr := addr
                    io.o_tlb_write_data := mem_data

                    // don't use
                    // io.o_mem_nwrite_data := S(0)
                }
            } .otherwise {
                // TLB hit
                return_value.rv := io.i_tlb_read_data
                // don't use left LATCH
                // io.o_tlb_read_en := False
                // io.o_tlb_write_en := False
                // io.o_tlb_addr := U(0)
                // io.o_tlb_write_data := S(0)
                // // mem
                // io.o_mem_addr := U(0)
                // io.o_mem_read_en := False
                // io.o_mem_write_en := False
                // io.o_mem_nwrite_data := S(0)
            }

            return return_value.rv // return
        }

        //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // sim mode
        if(sim_mode == true && len == 64) {
            // sim mode not use fpga blackBox

            // Logic SV39

            // MMU Logic /////////////////////////////////////////////////////////////////////////////////////////////////////////

            // data struct
            val dcache_commom = Mem(cacheDataStruct(), wordCount = c_cap) // commom cache

            val va = io.i_cache_addr(38 downto 0) // get VA 39-bits

            // level index
            // val level_index = Reg(SInt(3 bits)) init(2)

            // MMU fail 
            val mmu_fail = Reg(Bool()) init(false)

            // First read csr get satp.PPN

            val stap_PPN = io.i_csr_satp(43 downto 0) // satp.PPN data 44-bits


            // Get current privilege
            val cur_privilege = get_current_privilege(io.i_csr_mstatue, io.i_csr_sstatue, io.i_csr_ustatue, io.i_ret_inst_statue)

            // SV39 has Three Level

            // One Level //////////////////////////////////////////////////////////////////////////////////////////////////////

            // Get next level page table entry address
            // val one_level_PTE_addr = U(stap_PPN * S(4096) + S(va(38 downto 30)) * S(8))
            val one_level_PTE_addr = stap_PPN * U(4096) + va(38 downto 30) * U(8)
            io.o_one_level_pte_addr := one_level_PTE_addr(56 downto 0) // debug, remove by publish
            io.o_one_level_pte_satpPPN := stap_PPN // debug, remove by publish
            io.o_one_level_pte_VPN := va(38 downto 30) // debug, remove by publish
            // println("----------------------")
            // println(one_level_PTE_addr.asBits)
            // println("----------------------")
            // Get data on address
            val one_level_PTE_data = search_cache_address_commom_cache(dcache_commom, one_level_PTE_addr) // one_level_PTE_addr is 59-bits bigger than PA 56-bits
            io.o_one_level_pte_data := one_level_PTE_data // debug, remove before publish
            io.o_cache_onelevepte_data := dcache_commom(one_level_PTE_addr(PA - c_cap_tag - 1 downto 0)).data // debug, remove before publish
            io.o_cache_onelevepte_tag := dcache_commom(one_level_PTE_addr(PA - c_cap_tag - 1 downto 0)).tag // debug, remove before publish
            io.o_cahce_onelevepte_addr := one_level_PTE_addr(PA - c_cap_tag - 1 downto 0) // debug, remove before publish
            io.o_cache_onelevepte_tag_true := one_level_PTE_addr(PA - 1 downto PA - c_cap_tag) // debug, remove before publish
            // Check PTE is illegal
            // val one_level_error = test_PTE_is_illgal(one_level_PTE_data, U(0), level_index, io.i_csr_mstatue, cur_privilege)
            val one_level_error = test_PTE_is_illgal(one_level_PTE_data, U(0), S(2), io.i_csr_mstatue, cur_privilege)
            
            // final address //////////////////////
            case class finalAddrDataStruct() extends Bundle {
                val final_addr = UInt(PA bits)
            }

            val fa = new finalAddrDataStruct() // bundle saved final address after MMU

            // left LATCH
            fa.final_addr := U(0)
            ///////////////////////////////////////

            // Left LATCH
            io.o_csr_write_en := False
            io.o_csr_nwrite_addr := U(0)
            io.o_csr_nwrite_data := S(0)

            when(one_level_error =/= U"00") {
                // Has errors
                // Write ERROR
                write_error_to_csr(one_level_error, cur_privilege)
                mmu_fail := True // set fail flag
            } .otherwise {
                // don't has error
            ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

                // level -1
                // level_index := level_index - 1

            // Two Level //////////////////////////////////////////////////////////////////////////////////////////////////////

                // Get next level page table entry address
                // val two_level_PTE_addr = U(one_level_PTE_data(55 downto 12) * S(4096) + S(va(29 downto 21)) * S(8))
                val two_level_PTE_addr = U(one_level_PTE_data(55 downto 12)) * U(4096) + va(29 downto 21) * U(8)
                // Get data on address
                val two_level_PTE_data = search_cache_address_commom_cache(dcache_commom, two_level_PTE_addr)
                // Check PTE is illegal
                // val two_level_error = test_PTE_is_illgal(two_level_PTE_data, U(0), level_index, io.i_csr_mstatue, cur_privilege)
                val two_level_error = test_PTE_is_illgal(two_level_PTE_data, U(0), S(1), io.i_csr_mstatue, cur_privilege)

                when(two_level_error =/= U"00") {
                    // Has errors
                    // Write ERROR
                    write_error_to_csr(two_level_error, cur_privilege)
                    mmu_fail := True // set fail flag
                } .otherwise {
                    // Don't has error
            ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

                    // level -1
                    // level_index := level_index - 1

            // Three Level ////////////////////////////////////////////////////////////////////////////////////////////////////

                    // Get next level page table entry address
                    // val three_level_PTE_addr = U(two_level_PTE_data(55 downto 12) * S(4096) + S(va(20 downto 12)) * S(8))
                    val three_level_PTE_addr = U(two_level_PTE_data(55 downto 12)) * U(4096) + va(20 downto 12) * U(8)
                    // Get data on address
                    val three_level_PTE_data = search_cache_address_commom_cache(dcache_commom, three_level_PTE_addr)
                    // Check PTE is illegal
                    // val three_level_error = test_PTE_is_illgal(three_level_PTE_data, U(0), level_index, io.i_csr_mstatue, cur_privilege)
                    val three_level_error = test_PTE_is_illgal(three_level_PTE_data, U(0), S(0), io.i_csr_mstatue, cur_privilege)
                    
                    when(three_level_error =/= U"00") {
                        // Has error
                        // Write ERROR
                        write_error_to_csr(three_level_error, cur_privilege)
                        mmu_fail := True
                    } .otherwise {
                        // Don't has error
                        fa.final_addr := U(three_level_PTE_data(55 downto 0)) // No error, write address to Bundle // three_level_PTE_data is 64-bits bigger than 56-bits
                        mmu_fail := False // set fail flag
                    }
            /////////////////////////////////////////////////////////////////////// ////////////////////////////////////////////
                }
            }
            
            // io.o_level_index := level_index // test
            // Cache Logic ////////////////////////////////////////////////////////////////////////////////////////////////////

            io.o_nread_data := S(0) // left LATCH
            when(io.i_read_en && !io.i_write_en) {
                // read cache //////////////////////////////////////////////////////////////////////////////////////////////////

                // left LATCH
                // io.o_nread_data := S(0)

                // Logic
                when(io.i_csr_satp(63 downto 60) === U"0000") {
                    // close virtual memory
                    // val close_va_addr = U(Cat(U"00000000000000000", va))
                    val close_va_addr = io.i_cache_addr // direct use PA as addr

                    val read_data_tmp = dcache_commom(close_va_addr(PA - c_cap_tag - 1 downto 0)) // data in close_ca_addr

                    // left LATCH
                    // io.o_nread_data := S(0)

                    // logic
                    when(read_data_tmp.tag === close_va_addr(PA - 1 downto PA - c_cap_tag)) {
                        io.o_nread_data := read_data_tmp.data
                    } .otherwise {
                        io.o_nread_data := S(0) // error read
                    }
                } .elsewhen(mmu_fail === False && io.i_csr_satp(63 downto 60) === U(8)) {
                    // SV39 Mode
                    // Get Final PA
                    val pa = U(Cat(fa.final_addr(55 downto 12), va(11 downto 0)))

                    // read cache
                    
                    val read_data_tmp = dcache_commom(pa(PA - c_cap_tag - 1 downto 0)) // data in pa

                    // left LATCH
                    // io.o_nread_data := S(0)

                    // logic
                    when(read_data_tmp.tag === pa(PA - 1 downto PA - c_cap_tag)) {
                        io.o_nread_data := read_data_tmp.data
                    } .otherwise {
                        io.o_nread_data := S(0) // error read
                    }
                } .otherwise {
                    // Illegal
                    io.o_nread_data := S(0)
                }
            } .elsewhen(!io.i_read_en && io.i_write_en && mmu_fail === False) {
                // write cache /////////////////////////////////////////////////////////////////////////////////////////////

                val need_write_data = new cacheDataStruct()

                // left LATCH
                need_write_data.data := S(0)
                need_write_data.tag := U(0)
                need_write_data.use_flag := False

                // logic
                when(io.i_csr_satp(63 downto 60) === U(0)) {
                    // close virtual memory
                    // val close_va_addr = U(Cat(U"00000000000000000", va))
                    val close_va_addr = io.i_cache_addr // direct use PA as addr
                    need_write_data.data := io.i_nwrite_data
                    need_write_data.tag := close_va_addr(PA - 1 downto PA - c_cap_tag)
                    need_write_data.use_flag := True

                    // write
                    dcache_commom(close_va_addr(PA - c_cap_tag - 1 downto 0)) := need_write_data // write data
                } .elsewhen(mmu_fail === False && io.i_csr_satp(63 downto 60) === U(8)) {
                    // SV39 Mode
                    // Get Final PA
                    val pa = U(Cat(fa.final_addr(55 downto 12), va(11 downto 0)))

                    need_write_data.data := io.i_nwrite_data
                    need_write_data.tag := pa(PA - 1 downto PA - c_cap_tag)
                    need_write_data.use_flag := True

                    // write cache
                    dcache_commom(pa(PA - c_cap_tag - 1 downto 0)) := need_write_data
                } .otherwise {
                    // Illegal
                    val illegal_bots = U(0)
                }
            } .otherwise {
                // Illegal
                io.o_nread_data := S(0)
            }
        } else if(sim_mode == false) {
            // use fpga blackBox
        }
    } else if(c_mode == 1) {
        // 4 WAY cache with LRU
    }
}

object dcache {
    def main(args: Array[String]) {
        // Binary to Dec
        def binaryToDecWithOutRecur(src: String): Long = {
            // Convert bin ro dec
            val finalSrc = src.replaceAll("_", "")
            // println(finalSrc)

		    if(!finalSrc.matches("[0|1]*")){
		    	println("[Decoder Sim] INVALID INPUT .")
		    	return 0
		    }

		    val tmp = finalSrc.reverse
		    var res: Long = 0

		    for(i <- 0 to tmp.length - 1){
		    	res += tmp.substring(i, i + 1).toLong * (1 << i)
		    }
		    return res
	    }

        // 56-bits PA, 11-bits addr, 45-bits tag, 64-bits len, commom-cache Mode, 39-bits VA, 56-bits PA, sim-Mode enable
        SimConfig.withWave.compile(new dcache(0, 2048, 45, 64, 1, 39, 56, true)).doSim { dut =>
            dut.clockDomain.forkStimulus(period = 10) // time period

            // don't use mmu write ////////////////////////////////////////////////////////////////////////////////////
            // tlb
            // don't use mmu then no need tlb
            dut.io.i_tlb_read_data #= 0
            dut.io.i_tlb_statue #= false // True - TLB hit, False - TLB miss
            // Cache
            dut.io.i_cache_addr #= 666
            dut.io.i_read_en #= false
            dut.io.i_write_en #= true
            dut.io.i_nwrite_data #= 123456
            // Mem
            dut.io.i_mem_nread_data #= 0
            // CSR
            dut.io.i_csr_mstatue #= 0
            dut.io.i_csr_sstatue #= 0
            dut.io.i_csr_ustatue #= 0
            dut.io.i_csr_satp #= 0 // close mmu
            // Trap-return instruction statue
            dut.io.i_ret_inst_statue #= 0 // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET

            dut.clockDomain.waitSampling(1) // time wait

            // don't use mmu read /////////////////////////////////////////////////////////////////////////////////////
            // tlb
            // don't use mmu then no need tlb
            dut.io.i_tlb_read_data #= 0
            dut.io.i_tlb_statue #= false // True - TLB hit, False - TLB miss
            // Cache
            dut.io.i_cache_addr #= 666
            dut.io.i_read_en #= true
            dut.io.i_write_en #= false
            dut.io.i_nwrite_data #= 0
            // Mem
            dut.io.i_mem_nread_data #= 123456
            // CSR
            dut.io.i_csr_mstatue #= 0
            dut.io.i_csr_sstatue #= 0
            dut.io.i_csr_ustatue #= 0
            dut.io.i_csr_satp #= 0 // close mmu
            // Trap-return instruction statue
            dut.io.i_ret_inst_statue #= 0 // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET

            dut.clockDomain.waitSampling(1) // time wait

            // assert
            assert(dut.io.o_nread_data.toLong == 123456)

            // prepare mmu, use no mmu write one level PTE, S-MODE //////////////////////////////////////////////////////////////////////////
            // use mmu read VA : VPN[2] - 001001110 | VPN[1] - 111000001 | VPN[0] - 000011101 | OFFSET - 111000011110
            // satp.PPN[44] : 11110000111100011100010000001111001110000001
            // tlb
            // don't use mmu then no need tlb
            dut.io.i_tlb_read_data #= 0
            dut.io.i_tlb_statue #= false // True - TLB hit, False - TLB miss
            // Cache
            // dut.io.i_cache_addr #= binaryToDecWithOutRecur("11110000111100011100010000001111001110000001") * 4096 + binaryToDecWithOutRecur("001001110") * 8 // one level PTE address
            dut.io.i_cache_addr #= BigInt("67819818781774448") // one level PTE address
            dut.io.i_read_en #= false
            dut.io.i_write_en #= true
            // dut.io.i_nwrite_data #= binaryToDecWithOutRecur("0000000000_11100011100001111000111101_111000001_111000110_00_0_1_0_0_0_0_0_1") // PTE one
            dut.io.i_nwrite_data #= BigInt("16010967495153729") // PTE one
            // dut.io.i_nwrite_data #= ("16010967495153729").toLong // PTE one
            // Mem
            dut.io.i_mem_nread_data #= 0
            // CSR
            dut.io.i_csr_mstatue #= 0
            dut.io.i_csr_sstatue #= 0
            dut.io.i_csr_ustatue #= 0
            dut.io.i_csr_satp #= 0 // close mmu
            // Trap-return instruction statue
            dut.io.i_ret_inst_statue #= 0 // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET

            dut.clockDomain.waitSampling(1) // time wait

            // prepare mmu, use no mmu write two level PTE, S-MODE /////////////////////////////////////////////////////////////////////////
            dut.io.i_tlb_read_data #= 0
            dut.io.i_tlb_statue #= false // True - TLB hit, False - TLB miss
            // Cache
            // dut.io.i_cache_addr #= binaryToDecWithOutRecur("11100011100001111000111101_111000001_111000110") * 4096 + binaryToDecWithOutRecur("111000001") * 8 // one level PTE address
            dut.io.i_cache_addr #= BigInt("64043869980618248") // one level PTE address
            dut.io.i_read_en #= false
            dut.io.i_write_en #= true
            // dut.io.i_nwrite_data #= binaryToDecWithOutRecur("0000000000_00000011100001111000111101_101001001_100101011_00_0_1_0_0_0_0_0_1") // PTE two
            dut.io.i_nwrite_data #= BigInt("248368736283713") // PTE two
            // dut.io.i_nwrite_data #= ("248368736283713").toLong // PTE two
            // Mem
            dut.io.i_mem_nread_data #= 0
            // CSR
            dut.io.i_csr_mstatue #= 0
            dut.io.i_csr_sstatue #= 0
            dut.io.i_csr_ustatue #= 0
            dut.io.i_csr_satp #= 0 // close mmu
            // Trap-return instruction statue
            dut.io.i_ret_inst_statue #= 0 // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET

            dut.clockDomain.waitSampling(1) // time wait

            // prepare mmu, use no mmu write three level PTE, S-MODE ///////////////////////////////////////////////////////////////////
            dut.io.i_tlb_read_data #= 0
            dut.io.i_tlb_statue #= false // True - TLB hit, False - TLB miss
            // Cache
            // dut.io.i_cache_addr #= binaryToDecWithOutRecur("00000011100001111000111101_101001001_100101011") * 4096 + binaryToDecWithOutRecur("000011101") * 8 // one level PTE address
            dut.io.i_cache_addr #= BigInt("993474945134824") // one level PTE address
            dut.io.i_read_en #= false
            dut.io.i_write_en #= true
            // dut.io.i_nwrite_data #= binaryToDecWithOutRecur("0000000000_11000011100001111010100101_101001001_000110110_00_0_1_0_0_1_0_1_1") // PTE three
            dut.io.i_nwrite_data #= BigInt("13759195535431755") // PTE three
            // Mem
            dut.io.i_mem_nread_data #= 0
            // CSR
            dut.io.i_csr_mstatue #= 0
            dut.io.i_csr_sstatue #= 0
            dut.io.i_csr_ustatue #= 0
            dut.io.i_csr_satp #= 0 // close mmu
            // Trap-return instruction statue
            dut.io.i_ret_inst_statue #= 0 // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET

            dut.clockDomain.waitSampling(1) // time wait

            // prepare mmu, use no mmu write PA : 11000011100001111010100101101001001000110110 | OFFEST : 111000011110 ///////////////////////
            dut.io.i_tlb_read_data #= 0
            dut.io.i_tlb_statue #= false // True - TLB hit, False - TLB miss
            // Cache
            // dut.io.i_cache_addr #= binaryToDecWithOutRecur("11000011100001111010100101101001001000110110_111000011110") // 56-bits
            dut.io.i_cache_addr #= BigInt("55036782141730334") // 56-bits
            dut.io.i_read_en #= false
            dut.io.i_write_en #= true
            dut.io.i_nwrite_data #= 143858
            // Mem
            dut.io.i_mem_nread_data #= 0
            // CSR
            dut.io.i_csr_mstatue #= 0
            dut.io.i_csr_sstatue #= 0
            dut.io.i_csr_ustatue #= 0
            dut.io.i_csr_satp #= 0 // close mmu
            // Trap-return instruction statue
            dut.io.i_ret_inst_statue #= 0 // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET

            dut.clockDomain.waitSampling(1) // time wait

            // use mmu, S-MODE, VA : VPN[2] - 111001110 | VPN[1] - 111000001 | VPN[0] - 000011101 | OFFSET - 111000011110 /////////////////
            dut.io.i_tlb_read_data #= 0
            dut.io.i_tlb_statue #= false // True - TLB hit, False - TLB miss
            // Cache
            // dut.io.i_cache_addr #= (binaryToDecWithOutRecur("001001110_111000001_000011101_111000011110")).toLong // 39-bits
            dut.io.i_cache_addr #= ("84693605918").toLong // 39-bits
            dut.io.i_read_en #= true
            dut.io.i_write_en #= false
            dut.io.i_nwrite_data #= 0
            // Mem
            dut.io.i_mem_nread_data #= 143858
            // CSR
            dut.io.i_csr_mstatue #= binaryToDecWithOutRecur("01_00000000000") // set S-MODE, MPP : 01
            dut.io.i_csr_sstatue #= 0
            dut.io.i_csr_ustatue #= 0
            // dut.io.i_csr_satp #= binaryToDecWithOutRecur("1000_0000000000000000_11110000111100011100010000001111001110000001") // close mmu
            dut.io.i_csr_satp #= BigInt("9223388594427720577")// close mmu
            // Trap-return instruction statue
            dut.io.i_ret_inst_statue #= binaryToDecWithOutRecur("01") // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET

            dut.clockDomain.waitSampling(1) // time wait

            // assert(dut.io.o_nread_data.toLong == 143858)
        }
    }
}