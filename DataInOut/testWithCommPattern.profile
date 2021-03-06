{
    "application": "graph500-mpi-simple",
    "test-scenario": "scale12-edgefactor16",
    "execmd": "./server -filename /home/lva/NFS/Graph500/run-mpi-analysis/mpi-simple.bc -ip 127.0.0.1 -portno 12345 -mpi-data -mpi-stats ",
    "time": "2015-11-23T12:25:18Z",
    "threads": [
      {
        "thread_id": 0,
        "processor_id": 0,
        "instructionMix": {
          "instructions_analyzed": 112504838,
          "load_instructions": 11232632,
          "load_float_16bits_instructions": 0,
          "load_float_32bits_instructions": 0,
          "load_float_64bits_instructions": 94,
          "load_float_80bits_instructions": 0,
          "load_float_128bits_instructions": 0,
          "load_float_128bits_powerpc_instructions": 0,
          "load_float_64bits_mmx_instructions": 0,
          "load_int_4bits_instructions": 0,
          "load_int_8bits_instructions": 131508,
          "load_int_16bits_instructions": 0,
          "load_int_32bits_instructions": 1692630,
          "load_int_64bits_instructions": 8552353,
          "load_misc_instructions": 856047,
          "store_instructions": 6365265,
          "store_float_16bits_instructions": 0,
          "store_float_32bits_instructions": 0,
          "store_float_64bits_instructions": 40,
          "store_float_80bits_instructions": 0,
          "store_float_128bits_instructions": 0,
          "store_float_128bits_powerpc_instructions": 0,
          "store_float_64bits_mmx_instructions": 0,
          "store_int_4bits_instructions": 0,
          "store_int_8bits_instructions": 130658,
          "store_int_16bits_instructions": 0,
          "store_int_32bits_instructions": 400321,
          "store_int_64bits_instructions": 5834212,
          "store_misc_instructions": 34,
          "atomic_memory_instructions": 146613,
          "atomic_memory_cmpxchg_instructions": 0,
          "atomic_memory_rmw_instructions": 146613,
          "int_arith_instructions": 22735967,
          "int_arith_add_instructions": 8116769,
          "int_arith_sub_instructions": 2180984,
          "int_arith_mul_instructions": 5887820,
          "int_arith_udiv_instructions": 1,
          "int_arith_sdiv_instructions": 1511403,
          "int_arith_urem_instructions": 4187234,
          "int_arith_srem_instructions": 851756,
          "int_cmp_instructions": 17588582,
          "bitwise_instructions": 15089479,
          "bitwise_and_instructions": 6103649,
          "bitwise_or_instructions": 2393149,
          "bitwise_xor_instructions": 13,
          "bitwise_shift_left_instructions": 2717339,
          "bitwise_logical_shift_right_instructions": 3316709,
          "bitwise_arith_shift_right_instructions": 558620,
          "conversion_instructions": 4539537,
          "conversion_trunc_instructions": 1827838,
          "conversion_zext_instructions": 1703536,
          "conversion_sext_instructions": 1005675,
          "conversion_fptrunc_instructions": 0,
          "conversion_fpext_instructions": 0,
          "conversion_fptoui_instructions": 0,
          "conversion_fptosi_instructions": 2,
          "conversion_uitofp_instructions": 8,
          "conversion_sitofp_instructions": 12,
          "conversion_inttoptr_instructions": 0,
          "conversion_ptrtoint_instructions": 0,
          "conversion_bitcast_instructions": 2466,
          "conversion_address_space_cast_instructions": 0,
          "address_arith_instructions": 11766331,
          "address_arith_alloca_arith_instructions": 125,
          "address_arith_get_elem_ptr_arith_instructions": 11766206,
          "fp_arith_instructions": 98,
          "fp_arith_add_instructions": 34,
          "fp_arith_sub_instructions": 14,
          "fp_arith_mul_instructions": 31,
          "fp_arith_div_instructions": 19,
          "fp_arith_rem_instructions": 0,
          "fp_cmp_instructions": 8,
          "control_instructions": 20979954,
          "control_call_instructions": 2211741,
          "control_ret_instructions": 1909934,
          "control_branch_instructions": 16858279,
          "control_switch_instructions": 0,
          "control_indirectBr_instructions": 0,
          "control_invoke_instructions": 0,
          "control_resume_instructions": 0,
          "control_unreachable_instructions": 0,
          "phi_instructions": 0,
          "sync_instructions": 0,
          "sync_fence_instructions": 0,
          "vector_instructions": 0,
          "vector_extract_element_instructions": 0,
          "vector_insert_element_instructions": 0,
          "vector_shuffle_vector_instructions": 0,
          "aggregate_nonvector_instructions": 0,
          "aggregate_extract_value_instructions": 0,
          "aggregate_insert_value_instructions": 0,
          "misc_instructions": 2060372,
          "misc_select_instructions": 2060372,
          "misc_landingpad_instructions": 0,
          "misc_va_arg_instructions": 0
        },
        "mpiStatistics": {
          "MPI_instructions_analyzed": 301651,
          "environmentManagementRoutines": {
            "MPI_environment_management_calls": 14
          },
          "p2pCommunicationRoutines": {
            "MPI_Bsend": 0,
            "MPI_Bsend_init": 0,
            "MPI_Buffer_attach": 0,
            "MPI_Bsend_detach": 0,
            "MPI_Cancel": 0,
            "MPI_Get_count": 211,
            "MPI_Get_elements": 0,
            "MPI_Ibsend": 0,
            "MPI_Iprobe": 0,
            "MPI_Irecv": 211,
            "MPI_Irsend": 0,
            "MPI_Isend": 213,
            "MPI_Issend": 0,
            "MPI_Probe": 0,
            "MPI_Recv": 0,
            "MPI_Recv_init": 0,
            "MPI_Request_free": 0,
            "MPI_Rsend": 0,
            "MPI_Rsend_init": 0,
            "MPI_Send": 0,
            "MPI_Send_init": 0,
            "MPI_Sendrecv": 0,
            "MPI_Sendrecv_replace": 0,
            "MPI_Ssend": 0,
            "MPI_Ssend_init": 0,
            "MPI_Start": 0,
            "MPI_Startall": 0,
            "MPI_Test": 26485,
            "MPI_Test_cancelled": 0,
            "MPI_Testall": 0,
            "MPI_Testany": 0,
            "MPI_Testsome": 0,
            "MPI_Wait": 0,
            "MPI_Waitall": 0,
            "MPI_Waitany": 0,
            "MPI_Waitsome": 0
          },
          "collectiveCommunicationRoutines": {
            "MPI_Allgather": 0,
            "MPI_Allgatherv": 0,
            "MPI_Allreduce": 33,
            "MPI_Alltoall": 1,
            "MPI_Alltoallv": 1,
            "MPI_Barrier": 0,
            "MPI_Bcast": 0,
            "MPI_Gather": 0,
            "MPI_Gatherv": 0,
            "MPI_Op_create": 0,
            "MPI_Op_free": 0,
            "MPI_Reduce": 0,
            "MPI_Reduce_Scatter": 0,
            "MPI_Scan": 0,
            "MPI_Scatter": 0,
            "MPI_Scatterv": 0
          },
          "processGroupRoutines": {
            "MPI_Group_calls": 0
          },
          "communicatorsRoutines": {
            "MPI_Comm_calls": 5
          },
          "derivedTypesRoutines": {
            "MPI_Type_calls": 3
          },
          "virtualTopologyRoutines": {
            "MPI_virtual_topology_calls": 2
          },
          "miscellaneousRoutines": {
            "MPI_misc_calls": 0
          },
          "otherRoutines": {
            "MPI_other_calls": 274472
          }
        },
        "mpiDataStatistics": {
          "sent_bytes": 447904,
          "received_bytes": 864256,
          "sent_messages": 213,
          "received_messages": 211
        },
        "mpiCommunicationVector": [
          {
            "source": 1,
            "received_bytes": 126976,
            "received_messages": 31
          },
          {
            "source": 2,
            "received_bytes": 131072,
            "received_messages": 32
          },
          {
            "source": 3,
            "received_bytes": 114688,
            "received_messages": 28
          },
          {
            "source": 4,
            "received_bytes": 155648,
            "received_messages": 38
          },
          {
            "source": 5,
            "received_bytes": 102400,
            "received_messages": 25
          },
          {
            "source": 6,
            "received_bytes": 110592,
            "received_messages": 27
          },
          {
            "source": 7,
            "received_bytes": 122880,
            "received_messages": 30
          }
        ]
      }
    ]
}