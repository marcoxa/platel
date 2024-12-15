# CMakeLists.txt
# Top file
#
# See the file COPYING in the top directory for copyright and
# licensing information.

cmake_minimum_required(VERSION 3.30)

project(platel)

add_executable(platel_test c/platel_test.c)

add_subdirectory(c)

# add_library(platel_emacs_module platel_emacs_module.c platel.c)

target_link_libraries(platel_test PUBLIC platel_emacs_module)

target_include_directories(platel_test PUBLIC
  "${PROJECT_BINARY_DIR}"
  "${PROJECT_SOURCE_DIR/c}"
  "/Applications/Emacs.app/Contents/Resources/include"
)

target_include_directories(platel_emacs_module PUBLIC
  "${PROJECT_BINARY_DIR}"
  "${PROJECT_SOURCE_DIR/c}"
  "/Applications/Emacs.app/Contents/Resources/include"
)
    
# CMakeLists.txt ends here.
    
