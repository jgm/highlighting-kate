# The build procedure, after a fresh checkout from the repository:

#     make prep
#     cabal install
#
# 'make prep' generates the Haskell syntax parsers from kate
# xml syntax definitions.

XMLS=$(glob xml/*.xml)

.PHONY: prep all

all: prep
	cabal install -fexecutable

prep: clean ParseSyntaxFiles $(XMLS)
	./ParseSyntaxFiles xml
	@echo "Syntax parsers have been generated."
	@echo "You may now use cabal to build the package."

ParseSyntaxFiles: ParseSyntaxFiles.hs
	ghc --make -Wall ParseSyntaxFiles.hs  # requires HXT >= 9.0.0

clean:
	rm -rf Text/Highlighting/Kate/Syntax/*

distclean:
	rm -rf ParseSyntaxFiles.o ParseSyntaxFiles.hi ParseSyntaxFiles
