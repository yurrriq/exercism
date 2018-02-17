SRC=$(wildcard *.erl)

compile: $(SRC); @erlc $(SRC)

clean: ; @$(RM) *.beam

dialyzer: scrabble_score.erl; @dialyzer $<

doc: scrabble_score.erl
	@echo "Generating documentation for $< using EDoc..."
	@erl -noshell \
	     -run edoc_run files '["$<"]' '[{dir, "./doc"}, {private, true}]'

test: clean compile
	@erl -noshell -eval 'eunit:test(scrabble_score, [verbose])' -s init stop
