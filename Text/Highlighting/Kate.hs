module Text.Highlighting.Kate ( highlight, xhtmlHighlight, FormatOption (..) ) where
import Text.Highlighting.Kate.Format ( formatAsXHtml, FormatOption (..) )
import Text.Highlighting.Kate.Syntax ( highlight )
import Text.XHtml.Transitional

-- | Highlight source code in XHTML using specified syntax.
xhtmlHighlight :: [FormatOption] -- ^ Options
               -> String         -- ^ Name of syntax to use
               -> String         -- ^ Source code to highlight
               -> Html
xhtmlHighlight opts lang code =
  case highlight lang code of
       Right result -> formatAsXHtml opts lang result
       Left  _      -> pre $ thecode << code

