### Makefile

EMACS_MOD_PATH = /Applications/Emacs.app/Contents/Resources/include
EMACS_MOD_OBJS = platel_emacs_module.o platel.o

.PHONY: platel_emacs_module_lib
platel_emacs_module_lib: $(EMACS_MOD_OBJS)
	$(CC) -dynamiclib $(EMACS_MOD_OBJS) -o platel_emacs_module.dylib


platel.o: platel.c platel.h
	$(CC) -c platel.c


platel_emacs_module.o: platel.h platel_emacs_module.c
	$(CC) -c -I$(EMACS_MOD_PATH) platel_emacs_module.c


.PHONY: clean
clean:
	$(RM) $(EMACS_MOD_OBJS) platel_emacs_module.dylib

### end of file -- Makefile
