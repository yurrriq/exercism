SRC=$(wildcard *.erl)

compile: $(SRC)
	@erlc -pa . $(SRC)

clean:
	@$(RM) -r *.beam

test: clean compile
	@erl -noshell -eval 'eunit:test(trinary_tests, [verbose])' -s init stop
