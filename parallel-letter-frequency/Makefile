# -*- tab-width: 2 -*-

SRC = $(wildcard *.erl)

compile: $(SRC); @erlc $(SRC)

clean: ; @$(RM) *.beam

dialyzer: parallel_letter_frequency.erl; @dialyzer $<

doc: parallel_letter_frequency.erl doc/overview.edoc
	@echo "Generating documentation for $< using EDoc..."
	@erl -noshell -run edoc_run files '["$<"]' '[{dir, "./doc"}, {private, true}]'

test: clean compile
	@erl -noshell -eval 'eunit:test(parallel_letter_frequency, [verbose])' \
			 -s init stop
