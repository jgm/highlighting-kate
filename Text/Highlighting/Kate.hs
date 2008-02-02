module Text.Highlighting.Kate ( highlight, xhtmlHighlight ) where
import Text.Highlighting.Kate.Format ( formatAsXHtml )
import Text.Highlighting.Kate.Syntax ( highlight )
import Text.XHtml.Transitional

-- | Highlight source code in XHTML using specified syntax.
xhtmlHighlight :: Bool    -- ^ Number lines
               -> String  -- ^ Name of syntax to use
               -> String  -- ^ Source code to highlight
               -> Html
xhtmlHighlight numberLines lang code =
  case highlight lang code of
       Right result -> formatAsXHtml numberLines lang result
       Left  _      -> pre $ thecode << code

