if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -list -assume noold_ldout_format")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
endif()

list(APPEND fortran_src
  acftobs_qc.f
  indexc40.f
  input_acqc.f
  output_acqc_noprof.f
  output_acqc_prof.f
  pietc.f90
  pkind.f90
  pmat2.f90
  pmat3.f90
  pmat.f90
  prepacqc.f
  pspl.f90
  rdhedr.f
  sub2mem_mer.f
  sub2mem_um.f
  tranQCflags.f
)

set(exe_name prepobs_profacft)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  w3emc::w3emc_4
  bufr::bufr_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
