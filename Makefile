# The build procedure, after a fresh checkout from the repository:

#     make prep
#     cabal install
#
# 'make prep' generates the Haskell syntax parsers from kate
# xml syntax definitions.

XMLS=$(glob xml/*.xml)

.PHONY: prep all test clean distclean install prof

all: prep
	cabal configure -fexecutable --enable-tests
	cabal build

prof:
	cabal configure --enable-library-profiling --enable-executable-profiling --disable-optimization -fexecutable
	cabal build

prep: clean ParseSyntaxFiles $(XMLS)
	cabal install -fexecutable --enable-tests --only-dependencies
	./ParseSyntaxFiles xml
	@echo "Syntax parsers have been generated."
	@echo "You may now use cabal to build the package."

install:
	cabal install

test:
	cabal test

ParseSyntaxFiles: ParseSyntaxFiles.hs
	cabal install HXT
	ghc --make -Wall ParseSyntaxFiles.hs  # requires HXT >= 9.0.0

clean:
	rm -rf Text/Highlighting/Kate/Syntax/*

distclean:
	rm -rf ParseSyntaxFiles.o ParseSyntaxFiles.hi ParseSyntaxFiles
