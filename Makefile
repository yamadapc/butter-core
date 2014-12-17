default:
	cabal clean
	cabal configure
	cabal build

lint:
	hlint . --ignore="Parse error"

coverage:
	cabal clean
	cabal install --only-dep --enable-test --enable-library-coverage
	cabal configure --enable-tests --enable-library-coverage
	cabal build
	cabal test --show-details=always --test-option=--color

test:
	cabal clean
	cabal install --only-dep --enable-test
	cabal configure --enable-test
	cabal test
