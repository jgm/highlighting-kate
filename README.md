highlighting-kate
-----------------

A Haskell source code highlighting library, based
on Kate's syntax description files (http://kate-editor.org/),
now [part of the KDE Framework's "KTextEditor" component](http://kate-editor.org/2013/11/11/kate-on-5-the-future-of-ktexteditor-and-kate-part/).
It can produce both HTML and LaTeX output.

Currently, the following languages/formats are supported:
Abc Actionscript Ada Agda Alert Alert_indent Apache Asn1 Asp Awk
Bash Bibtex Boo C Changelog Clojure Cmake Coffee Coldfusion
Commonlisp Cpp Cs Css Curry D Diff Djangotemplate Dockerfile Dot
Doxygen Doxygenlua Dtd Eiffel Elixir Email Erlang Fasm Fortran
Fsharp Gcc Glsl Gnuassembler Go Hamlet Haskell Haxe Html Idris
Ini Isocpp Java Javadoc Javascript Json Jsp Julia Kotlin Latex
Lex Lilypond LiterateCurry LiterateHaskell Llvm Lua M4 Makefile
Mandoc Markdown Mathematica Matlab Maxima Mediawiki Metafont
Mips Modelines Modula2 Modula3 Monobasic Nasm Noweb Objectivec
Objectivecpp Ocaml Octave Opencl Pascal Perl Php Pike Postscript
Prolog Pure Python R Relaxng Relaxngcompact Rest Rhtml Roff Ruby
Rust Scala Scheme Sci Sed Sgml Sql SqlMysql SqlPostgresql Tcl
Tcsh Texinfo Verilog Vhdl Xml Xorg Xslt Xul Yacc Yaml Zsh

To install, use the [stack] tool:

    stack install

[stack]:  http://docs.haskellstack.org/en/stable/README/

Note:  If you have checked out the source from the git repository,
you will first need to do:

    make prep

which generates some of the needed source files from xml syntax
definitions.

To generate the documentation:

    stack haddock

To run the test suite:

    stack test

For an example of the use of the library, see highlighting-kate.hs.

By default, this installation method will install an executable,
`highlighting-kate`, along with the library.  Normally this is
put into `$HOME/.local/bin`.  To avoid creation of the
executable, use `--flag highlighting-kate:-executable` with the
`stack` commands above.

To run `highlighting-kate`, specify the language name using -s:

    highlighting-kate -s haskell highlighting-kate.hs > example.html

If you don't specify a language name, `highlighting-kate` will try to guess it
from the file extension.`highlighting-kate` can also be used as a pipe,
reading input from STDIN.  For other options,

    highlighting-kate --help

Styling is done using span tags.  The Highlight program will include
default styles in the generated HTML, unless a link to a CSS file is
provided using the '--css' option. Some sample CSS files can be found
in the css directory. These use generic class names (Normal, Keyword,
DataType, DecVal, BaseN, Float, Char, String, Comment, Function, Others,
Alert, Error). For more fine-grained highlighting, users may wish to
create their own CSS files that use language-specific classes.

The parsers in Text/Highlighting/Kate/Syntax were automatically generated
from the Kate syntax definitions in the xml directory. You may modify
the xml files in this directory, or add new ones, and then regenerate
the parsers by doing:

    make prep

Note that ParseSyntaxFiles.hs requires the HXT package (>= 9.0.0).
`make prep` should install this automatically.

To get the current Kate syntax highlighting files, clone the ktexteditor
repository:

    git clone git://anongit.kde.org/ktexteditor

The syntax definitions can then be found in

    src/syntax/data

There is information on the syntax highlighting definitions at
<https://docs.kde.org/stable5/en/applications/katepart/highlight.html>.  See also
<http://kate-editor.org/2005/03/24/writing-a-syntax-highlighting-file/>.

Thanks are due to all the authors of these syntax definitions.

Changes have been made to the following xml files (diffs have
been left in the directory, with .patch extensions):

- haskell.xml: Small changes to mapping of styles to token types.
- lua.xml:  Variables and constants highlighted as "normal", not keywords.
- perl.xml:  Small regex change due to differences in regex engines.
- php.xml:  Added fallthrough so `<?php` prefix not needed.
- tcsh.xml: Replace invalid character assignment(?) of regex '\s' with ' '.

- base report bugs on the GitHub issue tracker:
<https://github.com/jgm/highlighting-kate/issues>.

