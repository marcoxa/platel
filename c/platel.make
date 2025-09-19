### -*- Mode: Makefile -*-
###
### platel.make
###
### Makefile for vanilla UN*X installations.
###
### This split is necessary as the location of the Emacs 'include'
### directory is not a given.
###
### See the file COPYING in the top directory for copyright and
### licensing information.


### Emacs and ELisp macros.
###
### Decent defaults using ELisp 'invocation-directory'.

EMACS = emacs
EMACS_BATCH_EVAL = $(EMACS) -Q --batch --eval=

ELISP_INCLUDE = \
(file-name-as-directory \
 (expand-file-name "include" \
                   (file-name-parent-directory invocation-directory)))

EMACS_VERSION = $(shell $(EMACS_BATCH_EVAL)'(princ emacs-version)')

EMACS_MOD_INCLUDE = $(shell $(EMACS_BATCH_EVAL)'(princ $(ELISP_INCLUDE))')


### Source, object and library macros.

PLATEL_SRCS = platel_emacs_module.c platel.c
PLATEL_HDRS = $(PLATEL_SRCS:.c=.h)
PLATEL_OBJS = $(PLATEL_SRCS:.c=.o)
PLATEL_MOD_DLL = platel_emacs_module.so
PLATEL_MOD_LOAD_PATH = "../"


### Various macros.

RM = rm
COPY = cp
MOVE = mv


### Targets.

all: start $(PLATEL_MOD_DLL)
	@echo platel make: 'platel' building done.


start:
	@echo platel make: 'platel' building started.
	@echo platel make: MAKEDIR = $(MAKEDIR)
	@echo platel make: EMACS   = $(EMACS) $(EMACS_VERSION)
	@echo


$(PLATEL_MOD_DLL): $(PLATEL_OBJS)
	$(CC) -shared $(PLATEL_OBJS) -o $@


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
	- $(COPY) $(PLATEL_MOD_DLL) $(PLATEL_MOD_LOAD_PATH)
	- $(COPY) platel_test $(PLATEL_MOD_LOAD_PATH)


.PHONY: uninstall
uninstall:
	- $(RM) $(PLATEL_MOD_LOAD_PATH)$(PLATEL_MOD_DLL)
	- $(RM) $(PLATEL_MOD_LOAD_PATH)platel_test


.PHONY: clean
clean:
	$(RM) $(PLATEL_OBJS) platel_emacs_module.so platel_test

### end of file -- platel.make
