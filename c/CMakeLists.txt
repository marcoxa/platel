### CMakeLists.txt
### platel C source cmake file.
###
### See the file COPYING in the top directory for copyright and
### licensing information.


# Prologue.

message(STATUS "")
message(STATUS "C directory...")

### The library that will contain the module.

add_library(platel_emacs_module SHARED platel_emacs_module.c platel.c)

### add_library(platel_emacs_module MODULE platel_emacs_module.c platel.c)


### Ensure we do not add the 'lib' prefix.

set_target_properties(platel_emacs_module PROPERTIES PREFIX "")


# State that library needs PIC when the default is shared libraries.

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
  "${EMACS_MODULE_INCLUDE_DIR}"
)


### Installation instructions.
### Installing in the top directory, i.e., PROJECT_SOURCE_DIR.

install(TARGETS platel_emacs_module
  DESTINATION ${PROJECT_SOURCE_DIR}
  CONFIGURATIONS Debug
  RUNTIME COMPONENT Runtime
  LIBRARY COMPONENT Runtime NAMELINK_COMPONENT Development
)


### Epilogue.

message(STATUS "C PROJECT_BINARY_DIR   = ${PROJECT_BINARY_DIR}")
message(STATUS "C PROJECT_SOURCE_DIR/c = ${PROJECT_SOURCE_DIR}/c")
message(STATUS "C EMACS_MODULE_INCLUDE_DIR = ${EMACS_MODULE_INCLUDE_DIR}")
message(STATUS "INSTALL BIN DIR = ${CMAKE_INSTALL_BINDIR}")
message(STATUS "INSTALL LIB DIR = ${CMAKE_INSTALL_LIBDIR}")
message(STATUS "")

### CMakeLists.txt ends here
