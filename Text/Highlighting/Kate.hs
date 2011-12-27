{- |
   Module      : Text.Highlighting.Kate
   Copyright   : Copyright (C) 2008-2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports the main highlighting and formatting
functions.

A typical application will combine a highlighter and a formatter.
This one reads ruby code from stdin and writes HTML:

> import Text.Highlighting.Kate
> import Text.Blaze.Renderer.String (renderHtml)
> import Text.Blaze (toHtml)
> import Text.Blaze.Html5 as H
>
> main = do
>   code <- getContents
>   putStrLn $ renderHtml
>            $ do H.head (styleToHtml tango)
>                 H.body $ toHtml
>                        $ formatHtmlBlock defaultFormatOpts
>                        $ highlightAs "ruby" code

-}

module Text.Highlighting.Kate ( highlightAs
                              , languages
                              , languagesByExtension
                              , languagesByFilename
                              , highlightingKateVersion
                              -- * Basic types
                              , SourceLine
                              , Token
                              , TokenType (..)
                              , TokenStyle (..)
                              , Color (..)
                              -- * Formatting
                              , FormatOptions (..)
                              , formatHtmlInline
                              , formatHtmlBlock
                              , styleToHtml
                              , formatLaTeXInline
                              , formatLaTeXBlock
                              , styleToLaTeX
                              , defaultFormatOpts
                              -- * Styles
                              , Style (..)
                              , pygments
                              , espresso
                              , tango
                              , kate
                              , haddock
                              , monochrome
                              ) where
import Text.Highlighting.Kate.Format
import Text.Highlighting.Kate.Styles
import Text.Highlighting.Kate.Syntax
import Text.Highlighting.Kate.Definitions
import Data.Version (showVersion)
import Paths_highlighting_kate (version)

highlightingKateVersion = showVersion version
