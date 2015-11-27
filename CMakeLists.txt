cmake_minimum_required (VERSION 2.8)
project (slimc)

set (slimc_VERSION_MAJOR 1)
 
# add the executable
add_executable(slimc src/slimc.c)
add_executable(vm src/vm.c)
