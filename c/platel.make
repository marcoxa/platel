### Makefile
###
### Makefile for vanilla UN*X installations.
###
### This split is necessary ad the location of the Emacs 'include'
### directory is not a given.
###
### See the file COPYING in the top directory for copyright and
### licensing information.

EMACS_MOD_PATH = /Applications/Emacs.app/Contents/Resources/include
EMACS_MOD_OBJS = platel_emacs_module.o platel.o

.PHONY: platel_emacs_module_lib
platel_emacs_module_lib: $(EMACS_MOD_OBJS)
	$(CC) -shared $(EMACS_MOD_OBJS) -o platel_emacs_module.so


platel.o: platel.c platel.h
	$(CC) -FPIC -c platel.c


platel_emacs_module.o: platel.h platel_emacs_module.c
	$(CC) -FPIC -c -I$(EMACS_MOD_PATH) platel_emacs_module.c


platel_test: platel_test.c platel.o
	$(CC) -o platel_test platel_test.c platel.o


test: platel_test


.PHONY: clean
clean:
	$(RM) $(EMACS_MOD_OBJS) platel_emacs_module.so platel_test

### end of file -- Makefile
