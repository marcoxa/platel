### CMakeLists.txt
###
### See the file COPYING in the top directory for copyright and
### licensing information.

### The library that will contain the module.

add_library(platel_emacs_module SHARED platel_emacs_module.c platel.c)

### add_library(platel_emacs_module MODULE platel_emacs_module.c platel.c)


### Ensure we do not add the 'lib' prefix.

set_target_properties(platel_emacs_module PROPERTIES PREFIX "")

### CMakeLists.txt ends here
