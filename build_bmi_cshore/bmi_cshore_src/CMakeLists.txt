# bmi-cshore

cmake_minimum_required(VERSION 3.0)

project(bmi-cshore Fortran)

# in set (model_name, ...), it must be the same as the module in cshore.f
set(model_name cshore_module)

# in set(bmi_name, ...), it must be the same as the module name in bmi_cshore.f90 
set(bmi_name bmi_cshore_module)

# Determine the Fortran BMI version.
if(DEFINED ENV{BMIF_VERSION})
  set(bmif_version $ENV{BMIF_VERSION})
else()
  set(bmif_version "2.0")
endif()
string(REPLACE "." "_" bmif_module_version ${bmif_version})
message("-- BMIF version - ${bmif_version}")
message("-- BMIF module version - ${bmif_module_version}")

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

# Locate the installed Fortran BMI bindings (bmif library and module file)
# through CMAKE_PREFIX_PATH.
find_library(bmif_lib bmif)
find_path(bmif_inc bmif_${bmif_module_version}.mod)
include_directories(${bmif_inc})
message("-- bmif_lib - ${bmif_lib}")
message("-- bmif_inc - ${bmif_inc}")

add_subdirectory(cshore)
add_subdirectory(bmicshore)

#include(CTest)
