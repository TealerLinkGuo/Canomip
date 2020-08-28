package canomip.core

// Trans virtual address to physics address
def VA2PA(cache: Mem[cacheDataStruct], virtual_addr: UInt, stap_PPN: SInt, mstatue_data: SInt, privilege_now: UInt): UInt = {
    // search PTE from TLB, cache and memory

    val return_value = new Bundle { // return bundle
        val rv = UInt(3 bits)
    }

    val has_next_node = U(0) // param of test_PTE_is_illgal
    val no_next_node = U(1)
    var level_index = U(2)

    // SV39 has Three Level

    // One Level

    val one_level_PTE_addr = stap_PPN * U(4096) + virtual_addr(38 downto 30) * U(8) // get one level PTE addr
    // Search PTE from TLB, cache, memory
    val PTE_data_level_one = search_PTE_commom(cache, one_level_PTE_addr)
    // Check PTE is illegal
    val PTE_error_level_one = test_PTE_is_illgal(PTE_data_level_one, has_next_node, level_index, mstatue_data, privilege_now) // get error code

    // check is illegal
    when(PTE_error_level_one === U(0)) {
        // One level no error go to two level

        // Two Level

        val two_level_PTE_addr = U(PTE_data_level_one(55 downto 12)) * U(4096) + virtual_addr(29 downto 21) * U(8) // get two level PTE addr
        // Search PTE from TLB, cache, memory
        val PTE_data_level_two = search_PTE_commom(cache, two_level_PTE_addr)
        // Check PTE is illegal
        val PTE_error_level_two = test_PTE_is_illgal(PTE_data_level_two, has_next_node, level_index - U(1), mstatue_data, privilege_now) // get error code

        // check is illegal
        when(PTE_error_level_two === U(0)) {
            // Two level no error go to three level

            // Three Level

            val three_level_PTE_addr = U(PTE_data_level_two(55 downto 12)) * U(4096) + virtual_addr(20 downto 12) * U(8) // get three level PTE addr
            // Search PTE from TLB, cache, memory
            val PTE_data_level_three = search_PTE_commom(cache, three_level_PTE_addr)
            // Check PTE is illegal
            val PTE_error_level_three = test_PTE_is_illgal(PTE_data_level_three, has_next_node, level_index - U(1), mstatue_data, privilege_now) // get error code

            // check is illegal
            when(PTE_error_level_three === U(0)) {
                // write return value
                return_value.rv := U(PTE_data_level_three(55 downto 0)) // physics address
            } .otherwise {
                // write error
                write_error_CSR(PTE_error_level_three, privilege_now)
                return_value.rv := U(0)
            }
        } .otherwise {
            // write error
            write_error_CSR(PTE_error_level_two, privilege_now)
        }
    } .otherwise {
        // write error
        write_error_CSR(PTE_error_level_one, privilege_now)
    }

    return return_value.rv // return 
}