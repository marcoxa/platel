### CMakeLists.txt
### Top file
###
### See the file COPYING in the top directory for copyright and
### licensing information.

### Prologue

cmake_minimum_required(VERSION 3.30)

project(platel)


### Checking sytem/platform

message(STATUS "System is '${CMAKE_SYSTEM_NAME}'")
message(STATUS "UNIX is '${UNIX}'")

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin" AND
    EXISTS "/Applications/Emacs.app/") 
  ## We are on Mac OS and assume we are using the Emacs app, as
  ## installed by, e.g., homebrew from 'https://emacsformacosx.com/'
  
  set(EMACS_MODULE_INCLUDE_DIR
    "/Applications/Emacs.app/Contents/Resources/include")
  
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Windows" AND
    EXISTS "C:\\Program Files\\Emacs\\emacs-29.2\\include")
  ## We are on Windows with Emacs installed in a "standard" way.
  
  set(EMACS_MODULE_INCLUDE_DIR
    "C:\\Program Files\\Emacs\\emacs-29.2\\include")

elseif(${UNIX} AND
    EXISTS "/usr/local/share/emacs/emacs-29.2/include")
  ## We are on a UNI*X machine with Emacs installed in a "standard" way.
  
  set(EMACS_MODULE_INCLUDE_DIR
    "/usr/local/share/emacs/emacs-29.2/include")

else()
  message(FATAL_ERROR "Unknown system '${CMAKE_SYSTEM_NAME}'")

endif()

message(STATUS "EMACS include is '${EMACS_MODULE_INCLUDE_DIR}'")


### Declaring targets.

add_executable(platel_check c/platel_test.c)

add_subdirectory(c)

### add_library(platel_emacs_module platel_emacs_module.c platel.c)

target_link_libraries(platel_check PUBLIC platel_emacs_module)

target_include_directories(platel_check PUBLIC
  "${PROJECT_BINARY_DIR}"
  ## "${PROJECT_SOURCE_DIR}/c"
  "${EMACS_MODULE_INCLUDE_DIR}"
)

# target_include_directories(platel_emacs_module PUBLIC
#   "${PROJECT_BINARY_DIR}"
#   ## "${PROJECT_SOURCE_DIR}/c"
#   "${EMACS_MODULE_INCLUDE_DIR}"
# )


message(STATUS "PROJECT_BINARY_DIR   = ${PROJECT_BINARY_DIR}")
message(STATUS "PROJECT_SOURCE_DIR/c = ${PROJECT_SOURCE_DIR}/c")
message(STATUS "EMACS_MODULE_INCLUDE_DIR = ${EMACS_MODULE_INCLUDE_DIR}")

    
### CMakeLists.txt ends here.
    
