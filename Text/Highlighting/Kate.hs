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
>                         formatAsXHtml [OptNumberLines] "ruby" result
>         Left  err    -> error $ "Could not parse input: " ++ err

-}

module Text.Highlighting.Kate ( highlightAs
                              , languages
                              , languagesByExtension
                              , languagesByFilename
                              , formatAsXHtml
                              , FormatOption (..)
                              , defaultHighlightingCss
                              , SourceLine
                              , LabeledSource
                              , highlightingKateVersion
                              ) where
import Text.Highlighting.Kate.Format ( formatAsXHtml, FormatOption (..), defaultHighlightingCss )
import Text.Highlighting.Kate.Syntax ( highlightAs, languages, languagesByExtension, languagesByFilename )
import Text.Highlighting.Kate.Definitions ( SourceLine, LabeledSource )
import Data.Version (showVersion)
import Paths_highlighting_kate (version)

highlightingKateVersion = showVersion version
