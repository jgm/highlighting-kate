{- |
   Module      : Text.Highlighting.Kate
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

This helper module exports the main highlighting and formatting
functions.
  
A typical application will combine a highlighter and a formatter:

> main = do
>   code <- getContents
>   case highlightAs "ruby" code of
>         Right result -> putStrLn $ renderHtmlFragment $ 
>                         formatAsHtml [OptNumberLines] "ruby" result
>         Left  err    -> error $ "Could not parse input: " ++ err

-}

module Text.Highlighting.Kate ( highlightAs
                              , languages
                              , languagesByExtension
                              , languagesByFilename
                              , formatAsHtml
                              , formatAsLaTeX
                              , FormatOption (..)
                              , defaultHighlightingCss
                              , defaultLaTeXMacros
                              , SourceLine
                              , Token
                              , TokenType (..)
                              , TokenFormat (..)
                              , Color (..)
                              , Format (..)
                              , highlightingKateVersion
                              ) where
import Text.Highlighting.Kate.Format ( formatAsHtml, formatAsLaTeX, FormatOption (..), defaultHighlightingCss, defaultLaTeXMacros )
import Text.Highlighting.Kate.Syntax ( highlightAs, languages, languagesByExtension, languagesByFilename )
import Text.Highlighting.Kate.Definitions ( SourceLine, Token, TokenType(..) )
import Data.Version (showVersion)
import Paths_highlighting_kate (version)

highlightingKateVersion = showVersion version
