module Text.Highlighting.Kate.Syntax ( highlightAs, languages, languagesByExtension ) where
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Text.Highlighting.Kate.Definitions
import qualified Text.Highlighting.Kate.Syntax.Ada as Ada
import qualified Text.Highlighting.Kate.Syntax.Alert as Alert
import qualified Text.Highlighting.Kate.Syntax.Asp as Asp
import qualified Text.Highlighting.Kate.Syntax.Awk as Awk
import qualified Text.Highlighting.Kate.Syntax.Bash as Bash
import qualified Text.Highlighting.Kate.Syntax.Bibtex as Bibtex
import qualified Text.Highlighting.Kate.Syntax.C as C
import qualified Text.Highlighting.Kate.Syntax.Cmake as Cmake
import qualified Text.Highlighting.Kate.Syntax.Coldfusion as Coldfusion
import qualified Text.Highlighting.Kate.Syntax.Commonlisp as Commonlisp
import qualified Text.Highlighting.Kate.Syntax.Cpp as Cpp
import qualified Text.Highlighting.Kate.Syntax.Css as Css
import qualified Text.Highlighting.Kate.Syntax.D as D
import qualified Text.Highlighting.Kate.Syntax.Djangotemplate as Djangotemplate
import qualified Text.Highlighting.Kate.Syntax.Doxygen as Doxygen
import qualified Text.Highlighting.Kate.Syntax.Dtd as Dtd
import qualified Text.Highlighting.Kate.Syntax.Eiffel as Eiffel
import qualified Text.Highlighting.Kate.Syntax.Erlang as Erlang
import qualified Text.Highlighting.Kate.Syntax.Fortran as Fortran
import qualified Text.Highlighting.Kate.Syntax.Haskell as Haskell
import qualified Text.Highlighting.Kate.Syntax.Html as Html
import qualified Text.Highlighting.Kate.Syntax.Java as Java
import qualified Text.Highlighting.Kate.Syntax.Javadoc as Javadoc
import qualified Text.Highlighting.Kate.Syntax.Javascript as Javascript
import qualified Text.Highlighting.Kate.Syntax.Json as Json
import qualified Text.Highlighting.Kate.Syntax.Latex as Latex
import qualified Text.Highlighting.Kate.Syntax.Lex as Lex
import qualified Text.Highlighting.Kate.Syntax.LiterateHaskell as LiterateHaskell
import qualified Text.Highlighting.Kate.Syntax.Lua as Lua
import qualified Text.Highlighting.Kate.Syntax.Makefile as Makefile
import qualified Text.Highlighting.Kate.Syntax.Matlab as Matlab
import qualified Text.Highlighting.Kate.Syntax.Mediawiki as Mediawiki
import qualified Text.Highlighting.Kate.Syntax.Modula3 as Modula3
import qualified Text.Highlighting.Kate.Syntax.Nasm as Nasm
import qualified Text.Highlighting.Kate.Syntax.Objectivec as Objectivec
import qualified Text.Highlighting.Kate.Syntax.Ocaml as Ocaml
import qualified Text.Highlighting.Kate.Syntax.Octave as Octave
import qualified Text.Highlighting.Kate.Syntax.Pascal as Pascal
import qualified Text.Highlighting.Kate.Syntax.Perl as Perl
import qualified Text.Highlighting.Kate.Syntax.Php as Php
import qualified Text.Highlighting.Kate.Syntax.Postscript as Postscript
import qualified Text.Highlighting.Kate.Syntax.Prolog as Prolog
import qualified Text.Highlighting.Kate.Syntax.Python as Python
import qualified Text.Highlighting.Kate.Syntax.Relaxngcompact as Relaxngcompact
import qualified Text.Highlighting.Kate.Syntax.Rhtml as Rhtml
import qualified Text.Highlighting.Kate.Syntax.Ruby as Ruby
import qualified Text.Highlighting.Kate.Syntax.Scala as Scala
import qualified Text.Highlighting.Kate.Syntax.Scheme as Scheme
import qualified Text.Highlighting.Kate.Syntax.Sgml as Sgml
import qualified Text.Highlighting.Kate.Syntax.Sql as Sql
import qualified Text.Highlighting.Kate.Syntax.SqlMysql as SqlMysql
import qualified Text.Highlighting.Kate.Syntax.SqlPostgresql as SqlPostgresql
import qualified Text.Highlighting.Kate.Syntax.Tcl as Tcl
import qualified Text.Highlighting.Kate.Syntax.Texinfo as Texinfo
import qualified Text.Highlighting.Kate.Syntax.Xml as Xml
import qualified Text.Highlighting.Kate.Syntax.Xslt as Xslt
import qualified Text.Highlighting.Kate.Syntax.Yacc as Yacc

-- | List of supported languages.
languages :: [String]
languages = ["Ada","Alert","Asp","Awk","Bash","Bibtex","C","Cmake","Coldfusion","Commonlisp","Cpp","Css","D","Djangotemplate","Doxygen","Dtd","Eiffel","Erlang","Fortran","Haskell","Html","Java","Javadoc","Javascript","Json","Latex","Lex","LiterateHaskell","Lua","Makefile","Matlab","Mediawiki","Modula3","Nasm","Objectivec","Ocaml","Octave","Pascal","Perl","Php","Postscript","Prolog","Python","Relaxngcompact","Rhtml","Ruby","Scala","Scheme","Sgml","Sql","SqlMysql","SqlPostgresql","Tcl","Texinfo","Xml","Xslt","Yacc"]

-- | List of language extensions.
languageExtensions :: [(String, String)]
languageExtensions = [("Ada", Ada.syntaxExtensions), ("Alert", Alert.syntaxExtensions), ("Asp", Asp.syntaxExtensions), ("Awk", Awk.syntaxExtensions), ("Bash", Bash.syntaxExtensions), ("Bibtex", Bibtex.syntaxExtensions), ("C", C.syntaxExtensions), ("Cmake", Cmake.syntaxExtensions), ("Coldfusion", Coldfusion.syntaxExtensions), ("Commonlisp", Commonlisp.syntaxExtensions), ("Cpp", Cpp.syntaxExtensions), ("Css", Css.syntaxExtensions), ("D", D.syntaxExtensions), ("Djangotemplate", Djangotemplate.syntaxExtensions), ("Doxygen", Doxygen.syntaxExtensions), ("Dtd", Dtd.syntaxExtensions), ("Eiffel", Eiffel.syntaxExtensions), ("Erlang", Erlang.syntaxExtensions), ("Fortran", Fortran.syntaxExtensions), ("Haskell", Haskell.syntaxExtensions), ("Html", Html.syntaxExtensions), ("Java", Java.syntaxExtensions), ("Javadoc", Javadoc.syntaxExtensions), ("Javascript", Javascript.syntaxExtensions), ("Json", Json.syntaxExtensions), ("Latex", Latex.syntaxExtensions), ("Lex", Lex.syntaxExtensions), ("LiterateHaskell", LiterateHaskell.syntaxExtensions), ("Lua", Lua.syntaxExtensions), ("Makefile", Makefile.syntaxExtensions), ("Matlab", Matlab.syntaxExtensions), ("Mediawiki", Mediawiki.syntaxExtensions), ("Modula3", Modula3.syntaxExtensions), ("Nasm", Nasm.syntaxExtensions), ("Objectivec", Objectivec.syntaxExtensions), ("Ocaml", Ocaml.syntaxExtensions), ("Octave", Octave.syntaxExtensions), ("Pascal", Pascal.syntaxExtensions), ("Perl", Perl.syntaxExtensions), ("Php", Php.syntaxExtensions), ("Postscript", Postscript.syntaxExtensions), ("Prolog", Prolog.syntaxExtensions), ("Python", Python.syntaxExtensions), ("Relaxngcompact", Relaxngcompact.syntaxExtensions), ("Rhtml", Rhtml.syntaxExtensions), ("Ruby", Ruby.syntaxExtensions), ("Scala", Scala.syntaxExtensions), ("Scheme", Scheme.syntaxExtensions), ("Sgml", Sgml.syntaxExtensions), ("Sql", Sql.syntaxExtensions), ("SqlMysql", SqlMysql.syntaxExtensions), ("SqlPostgresql", SqlPostgresql.syntaxExtensions), ("Tcl", Tcl.syntaxExtensions), ("Texinfo", Texinfo.syntaxExtensions), ("Xml", Xml.syntaxExtensions), ("Xslt", Xslt.syntaxExtensions), ("Yacc", Yacc.syntaxExtensions)]

-- | Returns a list of languages appropriate for the given file extension.
languagesByExtension :: String -> [String]
languagesByExtension ext = filter (hasExtension ext) languages

-- | True if extension belongs to language.
hasExtension ext lang =
  let exts = fromMaybe "" (lookup lang languageExtensions)
      matchExtension _ [] = False
      matchExtension ext ('.':xs) =
        let (next, rest) = span (/=';') xs
        in  if next == ext then True else matchExtension ext rest
      matchExtension ext (_:xs) = matchExtension ext xs
  in  matchExtension (dropWhile (=='.') ext) exts

-- | Highlight source code using a specified syntax definition.
highlightAs :: String                        -- ^ Language syntax
            -> String                        -- ^ Source code to highlight
            -> Either String [SourceLine]    -- ^ Either error message or result
highlightAs lang =
  let lang'  = map toLower lang
      lang'' = if lang' `elem` map (map toLower) languages
                  then lang'
                  else case languagesByExtension lang' of
                            [l]  -> map toLower l  -- go by extension if unambiguous
                            _    -> lang'
  in  case lang'' of
        "ada" -> Ada.highlight
        "alert" -> Alert.highlight
        "asp" -> Asp.highlight
        "awk" -> Awk.highlight
        "bash" -> Bash.highlight
        "bibtex" -> Bibtex.highlight
        "c" -> C.highlight
        "cmake" -> Cmake.highlight
        "coldfusion" -> Coldfusion.highlight
        "commonlisp" -> Commonlisp.highlight
        "cpp" -> Cpp.highlight
        "css" -> Css.highlight
        "d" -> D.highlight
        "djangotemplate" -> Djangotemplate.highlight
        "doxygen" -> Doxygen.highlight
        "dtd" -> Dtd.highlight
        "eiffel" -> Eiffel.highlight
        "erlang" -> Erlang.highlight
        "fortran" -> Fortran.highlight
        "haskell" -> Haskell.highlight
        "html" -> Html.highlight
        "java" -> Java.highlight
        "javadoc" -> Javadoc.highlight
        "javascript" -> Javascript.highlight
        "json" -> Json.highlight
        "latex" -> Latex.highlight
        "lex" -> Lex.highlight
        "literatehaskell" -> LiterateHaskell.highlight
        "lua" -> Lua.highlight
        "makefile" -> Makefile.highlight
        "matlab" -> Matlab.highlight
        "mediawiki" -> Mediawiki.highlight
        "modula3" -> Modula3.highlight
        "nasm" -> Nasm.highlight
        "objectivec" -> Objectivec.highlight
        "ocaml" -> Ocaml.highlight
        "octave" -> Octave.highlight
        "pascal" -> Pascal.highlight
        "perl" -> Perl.highlight
        "php" -> Php.highlight
        "postscript" -> Postscript.highlight
        "prolog" -> Prolog.highlight
        "python" -> Python.highlight
        "relaxngcompact" -> Relaxngcompact.highlight
        "rhtml" -> Rhtml.highlight
        "ruby" -> Ruby.highlight
        "scala" -> Scala.highlight
        "scheme" -> Scheme.highlight
        "sgml" -> Sgml.highlight
        "sql" -> Sql.highlight
        "sqlmysql" -> SqlMysql.highlight
        "sqlpostgresql" -> SqlPostgresql.highlight
        "tcl" -> Tcl.highlight
        "texinfo" -> Texinfo.highlight
        "xml" -> Xml.highlight
        "xslt" -> Xslt.highlight
        "yacc" -> Yacc.highlight
        _ -> (\_ -> Left ("Unknown language: " ++ lang))
