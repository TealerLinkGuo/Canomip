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
        // read TLB
        val o_tlb_read_en = out Bool() // TLB read EN
        val o_tlb_read_addr = out UInt(VA bits) // out virtual address to TLB
        val i_tlb_read_data = in SInt(len bits) // input page table in TLB
        val i_tlb_statue = out Bool() // True - TLB hit, False - TLB miss
        // write TLB
        val o_tlb_write_en = out Bool() // TLB write EN
        val o_tlb_write_addr = out UInt(VA bits)
        val o_tlb_write_data = out SInt(len bits)

    // Cache RW
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

    // CSR access
        // read csr channel 1
        val o_csr_read_en = out Bool() // csr read EN
        val o_csr_nread_addr = out UInt(12 bits) // csr need read address
        val i_csr_nread_data = in SInt(len bits) // need satp.PPN value
        // read csr channe 2
        val o_csr_nread_addr_2 = out UInt(12 bits) // csr need read address
        val i_csr_nread_data_2 = in SInt(len bits) // need satp.PPN value
        // write csr
        val o_csr_write_en = out Bool() // csr write EN
        val o_csr_nwrite_addr = out UInt(12 bits) // csr need write address
        val o_csr_nwrite_data = out SInt(len bits) // csr need write data

    // Trap-return instruction statue
        val i_ret_inst_statue = in UInt(2 bits) // 00 - no exec, 01 - MRET, 10 - SRET, 11 - URET
    }

    // Logic

    def write_error_to_csr(error_code: UInt, privilege_now: UInt): Unit = {
        // write error to CSR REG

        // utval 0x043. stval 0x143

        // Logic 

        when(privilege_now === U"01") {
            // S-Mode

            // write
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
            }
        } .elsewhen(privilege_now === U"00") {
            // U-Mode

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
        }
        
    }

    def get_current_privilege(mstatue_data: SInt): UInt = {
        // Get privilege now
        // read csr reg sstatue or ustatue

        when(io.i_ret_inst_statue === U"01") {
            // MRET
            // io.o_csr_read_en := True
            // io.o_csr_nread_addr_2 := U(768) // mstatue in 0x300
            // val mstatue_data = io.i_csr_nread_data_2
            return U(mstatue_data(12 downto 11)) // mstatue MPP[1:0] (current privilege)
        } .elsewhen(io.i_ret_inst_statue === U"10") {
            // SRET
            io.o_csr_read_en := True
            io.o_csr_nread_addr_2 := U(256) // sstatue in 0x100
            val sstatue_data = io.i_csr_nread_data_2
            return U(sstatue_data(12 downto 11)) // sstatue MPP[1:0] (current privilege)
        } .elsewhen(io.i_ret_inst_statue === U"11") {
            // URET
            io.o_csr_read_en := True
            io.o_csr_nread_addr_2 := U(0) // sstatue in 0x000 
            val ustatue_data = io.i_csr_nread_data_2
            return U(ustatue_data(12 downto 11)) // ustatue MPP[1:0] (current privilege)
        } .otherwise {
            // no exec Trap-ret instruction
            return U"11" // default MRET
        }

        return U"10" // reserved value - default return value
    }

    def test_PTE_is_illgal(pte_data: SInt, node_now: UInt, level_index: SInt, mstatue_data: SInt, privilege_now: UInt): UInt = {
        // node_now - 0 : have next node, - 1 : don't have next node
        // return value 0 - no error,  retrun value 1 - 权限错误(rwx位于当前模式不符)
        // return value 2 - MXR error, return value 3 - 未对齐
        // return value 4 - 特权级错误, return value 5 - PTE无效
        // level_index - level number

        // test PTE data is illegal

        // get SUM and MXR
        val MXR = mstatue_data(19)
        val SUM = mstatue_data(18)

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
                                return U(0) // correct
                            } .otherwise {
                                return U(2) // MXR error
                            }
                        } .otherwise {
                            // success with pte.r = 1 && pte.x = 1
                            when(U(pte_data(1)) === U"1" && U(pte_data(3)) === U"1") {
                                return U(0) // correct
                            } .otherwise {
                                return U(2) // MXR error
                            }
                        }
                    } .otherwise {
                        return U(4) // 特权级错误
                    }
                } .elsewhen(privilege_now === U"00" && U(pte_data(4)) === U"1") {
                    // U-Mode
                    when(U(MXR) === U"1") {
                            // only success in pte.r = 1
                            when(U(pte_data(1)) === U"1" && U(pte_data(2)) =/= U"1" || U(pte_data(3)) =/= U"1") {
                                return U(0) // correct
                            } .otherwise {
                                return U(2) // MXR error
                            }
                        } .otherwise {
                            // success with pte.r = 1 && pte.x = 1
                            when(U(pte_data(1)) === U"1" && U(pte_data(3)) === U"1") {
                                return U(0) // correct
                            } .otherwise {
                                return U(2) // MXR error
                            }
                        }
                } .elsewhen(U(pte_data(6)) === U"0") {
                    // pte.a = 0
                    return U(5)
                }.otherwise {
                    return U(1) // 权限错误(rwx位于当前模式不符)
                }
            } .otherwise {
                return U(5) // PTE无效
            }
        } .elsewhen(level_index > S(0)) { // level_index > 0
            // have next node
            when(U(pte_data(0)) === U"1") {
                when(U(pte_data(1)) === U"0" && U(pte_data(2)) === U"1") {
                    return U(1) // 权限错误(rwx位于当前模式不符)
                } .otherwise {
                    return U(0) // correct
                }
            } .otherwise {
                return U(5) // PTE无效
            }
        } .otherwise { // level_index < 0
            // 未对齐
            return U(3)
        }

        // no error return 0
        return U(0)
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

            // First search TLB
            io.o_tlb_read_en := True // enable TLB
            io.o_tlb_read_addr := addr
            
            when(io.i_tlb_statue === False) {
                // TLB miss

                val cache_addr = addr(len - c_cap_tag downto 0)
                val cache_tag = addr(len - 1 downto len - c_cap_tag)

                // search cache
                val cache_data = cache(cache_addr)

                when(cache_data.tag === cache_tag && cache_data.use_flag === True) {
                    // write TLB
                    io.o_tlb_read_en := False // close TLB read
                    io.o_tlb_write_en := True
                    io.o_tlb_write_addr := addr
                    io.o_tlb_write_data := cache_data.data

                    return cache_data.data
                } .otherwise {
                    // cache miss

                    // mem access
                    io.o_mem_read_en := True
                    io.o_mem_read_addr := addr
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
                    io.o_tlb_write_addr := addr
                    io.o_tlb_write_data := mem_data
                }
            } .otherwise {
                // TLB hit
                return io.i_tlb_read_data
            }

            return S(0) // default return value
        }

        //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // sim mode
        if(sim_mode == true && len == 64) {
            // sim mode not use fpga blackBox

            // Logic

            // MMU Logic /////////////////////////////////////////////////////////////////////////////////////////////////////////

            // data struct
            val dcache_commom = Mem(cacheDataStruct(), wordCount = c_cap) // commom cache
            
            // read/write address convert
            case class vaDataStruct() extends Bundle {
                val va = UInt(39 bits)
            }

            val va_Bundle = new vaDataStruct() // save VA

            // convert
            when(io.i_read_en && !io.i_write_en) {
                va_Bundle.va := io.i_read_addr
            } .otherwise {
                va_Bundle.va := io.i_write_addr
            }

            val va = va_Bundle.va // get VA

            // level index
            val level_index = Reg(SInt(2 bits)) init(2)

            // MMU fail 
            val mmu_fail = Reg(Bool()) init(false)

            // First read csr get satp.PPN and mstatue

            io.o_csr_read_en := True // enable csr read
            io.o_csr_nread_addr := U(384) // satp in 0x180
            val stap_PPN = io.i_csr_nread_data(44 downto 0) // satp.PPN data

            io.o_csr_read_en := True
            io.o_csr_nread_addr_2 := U(768) // mstatue in 0x300
            val mstatue_data = io.i_csr_nread_data_2 // mstatue data

            // Get current privilege
            val cur_privilege = get_current_privilege(mstatue_data)

            // SV39 has Three Level

            // One Level //////////////////////////////////////////////////////////////////////////////////////////////////////

            // Get next level page table entry address
            val one_level_PTE_addr = U(stap_PPN * S(4096) + S(io.i_read_addr(38 downto 30)) * S(8))
            // Get data on address
            val one_level_PTE_data = search_cache_address_commom_cache(dcache_commom, one_level_PTE_addr)
            // Check PTE is illegal
            val one_level_error = test_PTE_is_illgal(one_level_PTE_data, U(0), level_index, mstatue_data, cur_privilege)
            
            // final address //////////////////////

            case class finalAddrDataStruct() extends Bundle {
                val final_addr = UInt(PA bits)
            }

            val fa = new finalAddrDataStruct() // bundle saved final address after MMU
            ///////////////////////////////////////
            when(one_level_error =/= U"00") {
                // Write ERROR
                write_error_to_csr(one_level_error, cur_privilege)
                mmu_fail := True
            } .otherwise {
                

            ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

                // level -1
                level_index := level_index - 1

            // Two Level //////////////////////////////////////////////////////////////////////////////////////////////////////

                // Get next level page table entry address
                val two_level_PTE_addr = U(one_level_PTE_data(55 downto 12) * S(4096) + S(va(29 downto 21)) * S(8))
                // Get data on address
                val two_level_PTE_data = search_cache_address_commom_cache(dcache_commom, two_level_PTE_addr)
                // Check PTE is illegal
                val two_level_error = test_PTE_is_illgal(two_level_PTE_data, U(0), level_index, mstatue_data, cur_privilege)

                when(two_level_error =/= U"00") {
                    // Write ERROR
                    write_error_to_csr(two_level_error, cur_privilege)
                    mmu_fail := True
                } .otherwise {
            ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

                    // level -1
                    level_index := level_index - 1

            // Three Level ////////////////////////////////////////////////////////////////////////////////////////////////////

                    // Get next level page table entry address
                    val three_level_PTE_addr = U(two_level_PTE_data(55 downto 12) * S(4096) + S(va(20 downto 12)) * S(8))
                    // Get data on address
                    val three_level_PTE_data = search_cache_address_commom_cache(dcache_commom, three_level_PTE_addr)
                    // Check PTE is illegal
                    val three_level_error = test_PTE_is_illgal(three_level_PTE_data, U(0), level_index, mstatue_data, cur_privilege)
                    
                    when(three_level_error =/= U"00") {
                        // Write ERROR
                        write_error_to_csr(three_level_error, cur_privilege)
                        mmu_fail := True
                    } .otherwise {
                        fa.final_addr := U(three_level_PTE_data) // No error, write address to Bundle
                        mmu_fail := False
                    }
            ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                }
            }
            

            // Cache Logic ////////////////////////////////////////////////////////////////////////////////////////////////////

            when(io.i_read_en && !io.i_write_en) {
                // read cache ///////////////////////////////////

                when(U(stap_PPN(63 downto 60)) === U(0)) {
                    // close virtual memory
                    val close_va_addr = U(Cat(U"00000000000000000", io.i_read_addr))

                    val read_data_tmp = dcache_commom(close_va_addr(PA - c_cap_tag - 1 downto 0)) // data in close_ca_addr

                    when(read_data_tmp.tag === close_va_addr(PA downto PA - c_cap_tag)) {
                        io.o_nread_data := read_data_tmp.data
                    } .otherwise {
                        io.o_nread_data := S(0) // error read
                    }
                } .elsewhen(mmu_fail === False && U(stap_PPN(63 downto 60)) === U(8)) {
                    // SV39 Mode
                    // Get Final PA
                    val pa = U(Cat(fa.final_addr(55 downto 12), io.i_read_addr(11 downto 0)))

                    // read cache
                    
                    val read_data_tmp = dcache_commom(pa(PA - c_cap_tag - 1 downto 0)) // data in pa

                    when(read_data_tmp.tag === pa(PA downto PA - c_cap_tag)) {
                        io.o_nread_data := read_data_tmp.data
                    } .otherwise {
                        io.o_nread_data := S(0) // error read
                    }
                } .otherwise {
                    // Illegal
                    io.o_nread_data := S(0)
                }
            } .elsewhen(!io.i_read_en && io.i_write_en && mmu_fail === False) {
                // write cache ///////////////////////////////////////

                val need_write_data = new cacheDataStruct()

                when(U(stap_PPN(63 downto 60)) === U(0)) {
                    // close virtual memory
                    val close_va_addr = U(Cat(U"00000000000000000", io.i_write_addr))
                    need_write_data.data := io.i_nwrite_data
                    need_write_data.tag := close_va_addr(PA downto PA - c_cap_tag)
                    need_write_data.use_flag := True

                    // write
                    dcache_commom(close_va_addr(PA - c_cap_tag - 1 downto 0)) := need_write_data // write data
                } .elsewhen(mmu_fail === False && U(stap_PPN(63 downto 60)) === U(8)) {
                    // SV39 Mode
                    // Get Final PA
                    val pa = U(Cat(fa.final_addr(55 downto 12), io.i_read_addr(11 downto 0)))

                    need_write_data.data := io.i_nwrite_data
                    need_write_data.tag := pa(PA downto PA - c_cap_tag)
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

            // TODO : sim code
        }
    }
}