TIX_FILE := $(shell stack path --local-hpc-root)/combined/custom/custom.tix
LCOV_FILE := ./coverage/lcov.info

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
	mkdir ./coverage;\
	stack exec -- hpc-lcov --file $(TIX_FILE) -o $(LCOV_FILE) --main-package madlib
