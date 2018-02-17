SRC=$(wildcard *.erl)

compile: $(SRC); @erlc $(SRC)

clean: ; @$(RM) *.beam

dialyzer: hello_world.erl; @dialyzer $<

doc: hello_world.erl
	@echo "Generating documentation for $< using EDoc..."
	@erl -noshell \
	     -run edoc_run files '["$<"]' '[{dir, "./doc"}, {private, true}]'

test: clean compile
	@erl -noshell -eval 'eunit:test(hello_world, [verbose])' -s init stop
