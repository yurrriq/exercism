ERL    := $(shell which erl)
REBAR3 := $(shell which rebar3)

ifeq ($(ERL),)
  $(error Can't find Erlang executable 'erl')
else ifeq ($(REBAR3),)
  $(error Can't find rebar3)
endif

compile: ; $(REBAR3) compile

clean: ; $(REBAR3) clean

.PHONY: test
test: ; $(REBAR3) eunit -m leap-tests
