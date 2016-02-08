SRC=$(wildcard *.erl)

compile: $(SRC)
	@erlc $(SRC)

clean:
	@$(RM) *.beam

dialyzer: largest_series_product.erl
	@dialyzer $<

doc: largest_series_product.erl
	@echo "Generating documentation for $< using EDoc..."
	@erl -noshell \
	     -run edoc_run files '["$<"]' '[{dir, "./doc"}, {private, true}]'

test: clean compile
	@erl -noshell \
	     -eval 'eunit:test(largest_series_product, [verbose])' \
	     -s init stop
