DOC_ROOT := $(shell stack path --local-doc-root)

.PHONY: doc
doc: ; stack build --haddock && rsync -vazP $(DOC_ROOT) ../_site/haskell/
