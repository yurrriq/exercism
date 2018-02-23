CFLAGS  = -std=c99
CFLAGS += -g
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -pedantic
CFLAGS += -Werror

VFLAGS  = --quiet
VFLAGS += --tool=memcheck
VFLAGS += --leak-check=full
VFLAGS += --error-exitcode=1

cpif   ?= | cpif
NW_SRC := $(wildcard src/*.nw)
C_SRC  := ${NW_SRC:.nw=.c}
PDF    := ${NW_SRC:.nw=.pdf}

latexmk_flags = -cd -pdf

ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags += -gg
endif


.PHONY: all test clean test/test_%.c

all: test

.SUFFIXES: .nw .c .out .tex .pdf

.nw.c:
	notangle $< ${cpif} $@
	indent -kr -nut $@

.nw.tex:
	@ noweave -autodefs c -n -delay -index $< ${cpif} $@

.tex.pdf:
	latexmk ${latexmk_flags} $<


all: test ${PDF}


clean:
	@ rm -fR **/*~ **/*.o **/*.out **/*.out.dSYM


src/%.out: src/%.c test/vendor/unity.c test/test_%.c
	@ rm -fR $@
	@ echo Compiling $@
	@ cc $(CFLAGS) $^ -o $@
	@ echo valgrind $(VFLAGS) $@
	@ $@


test: ${C_SRC:.c=.out}


test/test_%.c:
	@ sed -i '/TEST_IGNORE/d' $@
