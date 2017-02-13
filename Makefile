# The build procedure, after a fresh checkout from the repository:

#     make prep
#     stack install --test
#
# 'make prep' generates the Haskell syntax parsers from kate
# xml syntax definitions.

XMLS=$(glob xml/*.xml)

.PHONY: prep all test clean distclean install prof

all: prep
	stack build --install-ghc --test

prof:
	stack build --library-profiling --executable-profiling --fast

prep: clean $(XMLS)
	stack install --install-ghc hxt regex-posix utf8-string
	stack runghc ./ParseSyntaxFiles.hs xml
	@echo "Syntax parsers have been generated."
	@echo "You may now use cabal to build the package."

install:
	stack install

test:
	stack test

clean:
	rm -rf Text/Highlighting/Kate/Syntax/*

distclean:
	rm -rf ParseSyntaxFiles.o ParseSyntaxFiles.hi ParseSyntaxFiles
