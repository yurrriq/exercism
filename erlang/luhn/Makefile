EXERCISE  := $(notdir $(CURDIR))
dir       ?= src
module    ?= $(EXERCISE)
numtests  ?= 10000
proper     = proper -d $(dir) -m $(module) -n $(numtests)
eunit      = eunit

.PHONY: all clean compile dialyzer doc eunit proper test

all: test

clean: ; @rebar3 do clean

compile: ; @rebar3 compile

dialyzer: ; @rebar3 dialyzer

doc: ; @rebar3 edoc

eunit: ; @rebar3 $(eunit)

proper: ; @rebar3 $(proper)

test: clean; @rebar3 do $(proper), $(eunit)
