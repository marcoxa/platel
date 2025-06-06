### -*- Mode: Makefile -*-
###
### platel-darwin.make
###
### Makefile for Mac OS (darwin) assuming the emacsformacosx.com
### (which is also the one used by homebrew) installation.
###
### This split is necessary ad the location of the Emacs 'include'
### directory is not a given.
###
### See the file COPYING in the top directory for copyright and
### licensing information.


## Environment setup.
## I assume that some of the following macros (e.g., EMACS_VERSION)
## will be defined at 'namke' invocation time.

## The following should be self-explanatory.
## EMACS_MOD_LOAD_PATH is the "installation" folder, which in this
## case just the main, top 'platel' folder, preparing for 'm?elpa'


EMACS_VERSION  = 30.1
### EMACS_VERSION_DIR =
EMACS_MOD_INCLUDE = "/Applications/Emacs.app/Contents/Resources/include"
EMACS_MOD_LOAD_PATH = "../"

EMACS_MOD_SRCS = platel_emacs_module.c platel.c
EMACS_MOD_HDRS = platel_emacs_module.h platel.h
###  EMACS_MOD_DEFS = platel_emacs_module.def # Not necessary
EMACS_MOD_OBJS = $(EMACS_MOD_SRCS:.c=.o)
### EMACS_MOD_LIBS = $(EMACS_MOD_SRCS:.c=.lib)
### EMACS_MOD_EXPS = $(EMACS_MOD_SRCS:.c=.exp)
EMACS_MOD_DLL  = platel_emacs_module.dylib

EMACS_MOD_LIB  = ..

### Various macros.

RM = rm
COPY = cp
MOVE = mv


### Targets.

all: start $(EMACS_MOD_DLL)
	@echo platel make: 'platel' building done.


start:
	@echo platel make: 'platel' building started.
	@echo platel make: MAKEDIR = $(MAKEDDIR)
	@echo


$(EMACS_MOD_DLL): $(EMACS_MOD_OBJS)
	$(CC) -dynamiclib $(EMACS_MOD_OBJS) -o $@


platel.o: platel.c platel.h
	$(CC) -FPIC -c platel.c


platel_emacs_module.o: platel.h platel_emacs_module.c
	$(CC) -FPIC -c -I$(EMACS_MOD_INCLUDE) platel_emacs_module.c


platel_test: platel_test.c platel.o
	$(CC) -o platel_test platel_test.c platel.o


.PHONY: test
test: platel_test


.PHONY: install
install:
	- $(COPY) $(EMACS_MOD_DLL) $(EMACS_MOD_LIB)
	- $(COPY) platel_test $(EMACS_MOD_LIB)


.PHONY: uninstall
uninstall:
	- $(RM) $(EMACS_MOD_LIB)/$(EMACS_MOD_DLL)
	- $(RM) $(EMACS_MOD_LIB)/platel_test


.PHONY: clean
clean:
	- $(RM) $(EMACS_MOD_OBJS) $(EMACS_MOD_DLL) platel_test


### end of file -- platel-darwin.make
