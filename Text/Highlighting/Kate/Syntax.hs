module Text.Highlighting.Kate.Syntax ( highlight, languages ) where
import Text.Highlighting.Kate.Common (capitalize)
import Text.Highlighting.Kate.Definitions
import qualified Text.Highlighting.Kate.Syntax.Postscript as Postscript
import qualified Text.Highlighting.Kate.Syntax.Matlab as Matlab
import qualified Text.Highlighting.Kate.Syntax.Prolog as Prolog
import qualified Text.Highlighting.Kate.Syntax.Diff as Diff
import qualified Text.Highlighting.Kate.Syntax.Xml as Xml
import qualified Text.Highlighting.Kate.Syntax.Mediawiki as Mediawiki
import qualified Text.Highlighting.Kate.Syntax.Erlang as Erlang
import qualified Text.Highlighting.Kate.Syntax.Html as Html
import qualified Text.Highlighting.Kate.Syntax.Java as Java
import qualified Text.Highlighting.Kate.Syntax.Lex as Lex
import qualified Text.Highlighting.Kate.Syntax.Haskell as Haskell
import qualified Text.Highlighting.Kate.Syntax.Makefile as Makefile
import qualified Text.Highlighting.Kate.Syntax.Latex as Latex
import qualified Text.Highlighting.Kate.Syntax.Javascript as Javascript
import qualified Text.Highlighting.Kate.Syntax.Php as Php
import qualified Text.Highlighting.Kate.Syntax.Scala as Scala
import qualified Text.Highlighting.Kate.Syntax.Objectivec as Objectivec
import qualified Text.Highlighting.Kate.Syntax.SqlMysql as SqlMysql
import qualified Text.Highlighting.Kate.Syntax.Ocaml as Ocaml
import qualified Text.Highlighting.Kate.Syntax.Tcl as Tcl
import qualified Text.Highlighting.Kate.Syntax.Bash as Bash
import qualified Text.Highlighting.Kate.Syntax.Css as Css
import qualified Text.Highlighting.Kate.Syntax.Alert as Alert
import qualified Text.Highlighting.Kate.Syntax.Lua as Lua
import qualified Text.Highlighting.Kate.Syntax.Coldfusion as Coldfusion
import qualified Text.Highlighting.Kate.Syntax.D as D
import qualified Text.Highlighting.Kate.Syntax.Nasm as Nasm
import qualified Text.Highlighting.Kate.Syntax.Djangotemplate as Djangotemplate
import qualified Text.Highlighting.Kate.Syntax.Objectivecpp as Objectivecpp
import qualified Text.Highlighting.Kate.Syntax.Awk as Awk
import qualified Text.Highlighting.Kate.Syntax.Python as Python
import qualified Text.Highlighting.Kate.Syntax.Commonlisp as Commonlisp
import qualified Text.Highlighting.Kate.Syntax.Sgml as Sgml
import qualified Text.Highlighting.Kate.Syntax.C as C
import qualified Text.Highlighting.Kate.Syntax.Javadoc as Javadoc
import qualified Text.Highlighting.Kate.Syntax.Asp as Asp
import qualified Text.Highlighting.Kate.Syntax.Ruby as Ruby
import qualified Text.Highlighting.Kate.Syntax.Perl as Perl
import qualified Text.Highlighting.Kate.Syntax.Fortran as Fortran
import qualified Text.Highlighting.Kate.Syntax.Json as Json
import qualified Text.Highlighting.Kate.Syntax.Cpp as Cpp
import qualified Text.Highlighting.Kate.Syntax.Cmake as Cmake
import qualified Text.Highlighting.Kate.Syntax.SqlPostgresql as SqlPostgresql
import qualified Text.Highlighting.Kate.Syntax.Dtd as Dtd
import qualified Text.Highlighting.Kate.Syntax.Sql as Sql
import qualified Text.Highlighting.Kate.Syntax.Texinfo as Texinfo
import qualified Text.Highlighting.Kate.Syntax.Scheme as Scheme
import qualified Text.Highlighting.Kate.Syntax.Ada as Ada
import qualified Text.Highlighting.Kate.Syntax.Bibtex as Bibtex
import qualified Text.Highlighting.Kate.Syntax.Pascal as Pascal
import qualified Text.Highlighting.Kate.Syntax.Yacc as Yacc
import qualified Text.Highlighting.Kate.Syntax.Doxygen as Doxygen
import qualified Text.Highlighting.Kate.Syntax.LiterateHaskell as LiterateHaskell
import qualified Text.Highlighting.Kate.Syntax.Xslt as Xslt

-- | List of supported languages.
languages :: [String]
languages = ["Postscript","Matlab","Prolog","Diff","Xml","Mediawiki","Erlang","Html","Java","Lex","Haskell","Makefile","Latex","Javascript","Php","Scala","Objectivec","SqlMysql","Ocaml","Tcl","Bash","Css","Alert","Lua","Coldfusion","D","Nasm","Djangotemplate","Objectivecpp","Awk","Python","Commonlisp","Sgml","C","Javadoc","Asp","Ruby","Perl","Fortran","Json","Cpp","Cmake","SqlPostgresql","Dtd","Sql","Texinfo","Scheme","Ada","Bibtex","Pascal","Yacc","Doxygen","LiterateHaskell","Xslt"]

-- | Highlight source code using a specified syntax definition.
highlight :: String                        -- ^ Language syntax
          -> String                        -- ^ Source code to highlight
          -> Either String [SourceLine]    -- ^ Either error message or result
highlight lang =
  case (capitalize lang) of
        "Postscript" -> Postscript.highlight
        "Matlab" -> Matlab.highlight
        "Prolog" -> Prolog.highlight
        "Diff" -> Diff.highlight
        "Xml" -> Xml.highlight
        "Mediawiki" -> Mediawiki.highlight
        "Erlang" -> Erlang.highlight
        "Html" -> Html.highlight
        "Java" -> Java.highlight
        "Lex" -> Lex.highlight
        "Haskell" -> Haskell.highlight
        "Makefile" -> Makefile.highlight
        "Latex" -> Latex.highlight
        "Javascript" -> Javascript.highlight
        "Php" -> Php.highlight
        "Scala" -> Scala.highlight
        "Objectivec" -> Objectivec.highlight
        "SqlMysql" -> SqlMysql.highlight
        "Ocaml" -> Ocaml.highlight
        "Tcl" -> Tcl.highlight
        "Bash" -> Bash.highlight
        "Css" -> Css.highlight
        "Alert" -> Alert.highlight
        "Lua" -> Lua.highlight
        "Coldfusion" -> Coldfusion.highlight
        "D" -> D.highlight
        "Nasm" -> Nasm.highlight
        "Djangotemplate" -> Djangotemplate.highlight
        "Objectivecpp" -> Objectivecpp.highlight
        "Awk" -> Awk.highlight
        "Python" -> Python.highlight
        "Commonlisp" -> Commonlisp.highlight
        "Sgml" -> Sgml.highlight
        "C" -> C.highlight
        "Javadoc" -> Javadoc.highlight
        "Asp" -> Asp.highlight
        "Ruby" -> Ruby.highlight
        "Perl" -> Perl.highlight
        "Fortran" -> Fortran.highlight
        "Json" -> Json.highlight
        "Cpp" -> Cpp.highlight
        "Cmake" -> Cmake.highlight
        "SqlPostgresql" -> SqlPostgresql.highlight
        "Dtd" -> Dtd.highlight
        "Sql" -> Sql.highlight
        "Texinfo" -> Texinfo.highlight
        "Scheme" -> Scheme.highlight
        "Ada" -> Ada.highlight
        "Bibtex" -> Bibtex.highlight
        "Pascal" -> Pascal.highlight
        "Yacc" -> Yacc.highlight
        "Doxygen" -> Doxygen.highlight
        "LiterateHaskell" -> LiterateHaskell.highlight
        "Xslt" -> Xslt.highlight
        _ -> (\_ -> Left ("Unknown language ++ " ++ lang))
