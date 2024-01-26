cpif   ?= | cpif
NW_SRC := $(wildcard src/*.nw)
C_SRC  := ${NW_SRC:.nw=.c}
PDF    := ${NW_SRC:.nw=.pdf}

CFLAGS += -Wmissing-declarations
CFLAGS += -DUNITY_SUPPORT_64 -DUNITY_OUTPUT_COLOR

latexmk_flags = -cd -pdf

ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags += -gg
endif

.PHONY: all test clean test/test_%.c

all: test ${PDF}

.PHONY: test

test: ${C_SRC:.c=.out}

test/test_%.c:
	@ sed -i '/TEST_IGNORE/d' $@

src/%.out: src/%.c test-framework/unity.c test/test_%.c
	@ rm -fR $@
	@ echo Compiling $@
	@ cc $(CFLAGS) $^ -o $@
	@ $@

.SUFFIXES: .nw .c .out .tex .pdf

.nw.c:
	notangle $< ${cpif} $@

.nw.tex:
	@ noweave -autodefs c -n -delay -index $< ${cpif} $@

.tex.pdf:
	@ latexmk ${latexmk_flags} $<
