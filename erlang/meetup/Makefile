SRC=$(wildcard *.erl)

compile: $(SRC); @erlc $(SRC)

clean: ; @$(RM) *.beam

dialyzer: meetup.erl; @dialyzer $<

doc: meetup.erl
	@echo "Generating documentation for $< using EDoc..."
	@erl -noshell \
	     -run edoc_run files '["$<"]' '[{dir, "./doc"}, {private, true}]'

test: clean compile
	@erl -noshell -eval 'eunit:test(meetup, [verbose])' -s init stop
