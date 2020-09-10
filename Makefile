lint:
	hlint src

prettify:
	stylish-haskell -r -v -i src/*.hs &&  stylish-haskell -r -v -i test/*.hs

