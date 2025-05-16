### CMakeLists.txt
### platel top cmake file.
###
### See the file COPYING in the top directory for copyright and
### licensing information.

### Prologue

cmake_minimum_required(VERSION 3.30)

project(platel VERSION 0.42)


### Checking sytem/platform

message(STATUS "System is '${CMAKE_SYSTEM_NAME}'")
message(STATUS "UNIX is '${UNIX}'")


### Set the C standard.

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED True)


### Emacs setup.

find_program(EMACS NAMES emacs.exe emacs)

if(${EMACS} STREQUAL "EMACS-NOTFOUND")
  message(FATAL_ERROR "Emacs cannot be found in default paths.")
endif()

message(STATUS "Emacs is '${EMACS}'")

set(EMACS_VERSION 29.2)         # Minimum default.


# CMake 'execute_process' semantics requires "raw" strings passed to
# the command processor.  Hence the two examples below don't work, but
# the code does.
# Three hours to figure this out!
#
# emacs -Q --batch [[--eval='(progn (princ emacs-version) emacs-version)']]
# emacs -Q --batch "--eval='(princ emacs-version)'"

execute_process(COMMAND ${EMACS} -Q --batch [[--eval=(princ emacs-version)]]
  OUTPUT_VARIABLE EMACS_VERSION
  COMMAND_ERROR_IS_FATAL ANY
)

message(STATUS "Emacs version is '${EMACS_VERSION}'")


if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin" AND
    EXISTS "/Applications/Emacs.app/") 
  ## We are on Mac OS and assume we are using the Emacs app, as
  ## installed by, e.g., homebrew from 'https://emacsformacosx.com/'
  
  set(EMACS_MODULE_INCLUDE_DIR
    "/Applications/Emacs.app/Contents/Resources/include/")
  
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Windows" AND
    EXISTS "C:\\Program Files\\Emacs\\emacs-${EMACS_VERSION}\\include\\")
  ## We are on Windows with Emacs installed in a "standard" way.
  
  set(EMACS_MODULE_INCLUDE_DIR
    "C:\\Program Files\\Emacs\\emacs-${EMACS_VERSION}\\include\\")

elseif(${UNIX} AND
    EXISTS "/usr/local/share/emacs/emacs-${EMACS_VERSION}/include/")
  ## We are on a UNI*X machine with Emacs installed in a "standard" way.
  
  set(EMACS_MODULE_INCLUDE_DIR
    "/usr/local/share/emacs/emacs-${EMACS_VERSION}/include/")

elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin" OR
    ${CMAKE_SYSTEM_NAME} STREQUAL "Windows" OR
    ${UNIX})
  
  message(FATAL_ERROR "Emacs include dir not found.")
else()
  message(FATAL_ERROR "Unknown system '${CMAKE_SYSTEM_NAME}'.")
endif()

message(STATUS "Emacs include dir is '${EMACS_MODULE_INCLUDE_DIR}'")


### Global options.

### Ensure that the .dll is completely built if we are on Windows.

set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS True)

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

message(STATUS "Building shared libraries: ${BUILD_SHARED_LIBS}")


### Declaring targets.

add_subdirectory(c)

add_library(platel_obj OBJECT c/platel.c)

add_executable(platel_test c/platel_test.c $<TARGET_OBJECTS:platel_obj>)


### Declaring shared libraries destinations.

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")

target_link_libraries(platel_test PUBLIC platel_obj)

target_include_directories(platel_test PUBLIC "${PROJECT_BINARY_DIR}")


### Installation instructions.
### Installing in the top directory, i.e., PROJECT_SOURCE_DIR.

install(TARGETS platel_test
  DESTINATION ${PROJECT_SOURCE_DIR}
  CONFIGURATIONS Debug
)


### Epilogue.

message(STATUS "Epilogue...")
message(STATUS "CMAKE_BINARY_DIR   = ${CMAKE_BINARY_DIR}")
message(STATUS "CMAKE_SOURCE_DIR   = ${CMAKE_SOURCE_DIR}")
message(STATUS "PROJECT_BINARY_DIR   = ${PROJECT_BINARY_DIR}")
message(STATUS "PROJECT_SOURCE_DIR/c = ${PROJECT_SOURCE_DIR}/c")
message(STATUS "EMACS_MODULE_INCLUDE_DIR = ${EMACS_MODULE_INCLUDE_DIR}")
message(STATUS "CMAKE_INSTALL_PREFIX = ${CMAKE_INSTALL_PREFIX}")
message(STATUS "CMAKE_INCLUDE_PATH = ${CMAKE_INCLUDE_PATH}")
message(STATUS "CMAKE_LIBRARY_PATH = ${CMAKE_LIBRARY_PATH}")
message(STATUS "CMAKE_PREFIX_PATH  = ${CMAKE_PREFIX_PATH}")


### CMakeLists.txt ends here.
