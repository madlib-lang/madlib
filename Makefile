TIX_FILE := $(shell stack path --local-hpc-root)/combined/custom/custom.tix

lint:
	hlint src

prettify:
	stylish-haskell -r -v -i src/*.hs &&  stylish-haskell -r -v -i test/*.hs

test:
	stack test

test-watch:
	stack test --file-watch --fast

test-coverage:
	stack test --coverage && stack hpc report .

coverage-lcov:
	stack exec -- hpc-lcov --file $(TIX_FILE) --main-package madlib
