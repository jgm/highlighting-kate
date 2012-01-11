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

> import Text.Highlighting.Kate (highlightAs)
> import Text.Highlighting.Kate.Styles (tango)
> import Text.Highlighting.Kate.Format.HTML (formatHtmlBlock, styleToHtml)
> import Text.Blaze.Renderer.String (renderHtml)
> import Text.Blaze (toHtml)
> import Text.Blaze.Html5 as H
>
> main = do
>   code <- getContents
>   putStrLn $ renderHtml
>            $ do H.head (styleToHtml tango)
>                 H.body $ toHtml
>                        $ formatBlock defaultFormatOpts
>                        $ highlightAs "ruby" code

-}

module Text.Highlighting.Kate ( highlightAs
                              , languages
                              , languagesByExtension
                              , languagesByFilename
                              , highlightingKateVersion,
                              module Text.Highlighting.Kate.Types
                              ) where
import Text.Highlighting.Kate.Syntax
import Text.Highlighting.Kate.Types
import Data.Version (showVersion)
import Paths_highlighting_kate (version)

highlightingKateVersion = showVersion version
