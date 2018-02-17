SRC=$(wildcard *.erl)

compile: $(SRC); @erlc $(SRC)

clean: ; @$(RM) *.beam

dialyzer: triangle.erl; @dialyzer $<

doc: triangle.erl
	@echo "Generating documentation for $< using EDoc..."
	@erl -noshell \
	     -run edoc_run files '["$<"]' '[{dir, "./doc"}, {private, true}]'

test: clean compile
	@erl -noshell -eval 'eunit:test(triangle, [verbose])' -s init stop
