### CMakeLists.txt
###
### See the file COPYING in the top directory for copyright and
### licensing information.


message(STATUS "C directory")

### The library that will contain the module.

		      

### add_library(platel_emacs_module SHARED platel_emacs_module.c platel.c)
add_library(platel_emacs_module SHARED platel_emacs_module.c platel.c)

### add_library(platel_emacs_module MODULE platel_emacs_module.c platel.c)


### Ensure we do not add the 'lib' prefix.

set_target_properties(platel_emacs_module PROPERTIES PREFIX "")


# State that SqrtLibrary need PIC when the default is shared libraries.

set_target_properties(platel_emacs_module PROPERTIES
  POSITION_INDEPENDENT_CODE ${BUILD_SHARED_LIBS}
  )


### Set the include directory here.

# target_include_directories(platel_emacs_module
#   INTERFACE
#   ${CMAKE_CURRENT_SOURCE_DIR}
#   ## "${EMACS_MODULE_INCLUDE_DIR}"
# )

target_include_directories(platel_emacs_module
  PUBLIC
  ## "${PROJECT_BINARY_DIR}"
  ## "${PROJECT_SOURCE_DIR}/c"
  "${EMACS_MODULE_INCLUDE_DIR}"
)


### Epilogue

message(STATUS "C PROJECT_BINARY_DIR   = ${PROJECT_BINARY_DIR}")
message(STATUS "C PROJECT_SOURCE_DIR/c = ${PROJECT_SOURCE_DIR}/c")
message(STATUS "C EMACS_MODULE_INCLUDE_DIR = ${EMACS_MODULE_INCLUDE_DIR}")

### CMakeLists.txt ends here
