module Text.Highlighting.Kate.Format ( formatAsXHtml ) where
import Text.Highlighting.Kate.Definitions
import Text.XHtml.Transitional
import Text.Printf

-- | Format a list of highlighted @SourceLine@s as XHtml.
formatAsXHtml :: Bool         -- ^ Number lines?
              -> String       -- ^ Language definition to use
              -> [SourceLine] -- ^ Source lines to format
              -> Html
formatAsXHtml numberLines lang lines =
  pre ! [theclass $ unwords $ ["sourceCode", lang] ++ if numberLines then ["numberLines"] else []] $ thecode << map sourceLineToHtml lines

labeledSourceToHtml :: LabeledSource -> Html
labeledSourceToHtml (syntaxType, txt) =
  let classes = unwords $ filter (/= "") $ map removeSpaces syntaxType
  in if null classes 
        then toHtml txt
        else thespan ! [theclass classes, title classes ] << txt 

sourceLineToHtml :: SourceLine -> Html
sourceLineToHtml (SourceLine num contents) =
  concatHtml $ (thespan ! [theclass "LineNumber"] << (formatLineNumber num)) : (map labeledSourceToHtml contents) ++ [br]

formatLineNumber :: Int -> String
formatLineNumber = printf "%4d "

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')
