module Text.Highlighting.Kate.Format ( formatAsXHtml, FormatOption (..) ) where
import Text.Highlighting.Kate.Definitions
import Text.XHtml.Transitional
import Text.Printf

-- | Options for formatters.
data FormatOption = OptNumberLines     -- ^ Number lines
                  | OptTitleAttributes -- ^ Include title attributes
                  deriving (Eq, Show, Read)

-- | Format a list of highlighted @SourceLine@s as XHtml.
formatAsXHtml :: [FormatOption]  -- ^ Options
              -> String          -- ^ Language definition to use
              -> [SourceLine]    -- ^ Source lines to format
              -> Html
formatAsXHtml opts lang lines =
  pre ! [theclass $ unwords $ ["sourceCode", lang] ++ if OptNumberLines `elem` opts then ["numberLines"] else []] $
        thecode << map (sourceLineToHtml opts) lines

labeledSourceToHtml :: [FormatOption] -> LabeledSource -> Html
labeledSourceToHtml opts (syntaxType, txt) =
  let classes = unwords $ filter (/= "") $ map removeSpaces syntaxType
  in if null classes 
        then toHtml txt
        else let attribs = [theclass classes] ++ if OptTitleAttributes `elem` opts then [title classes] else []
             in  thespan ! attribs << txt 

sourceLineToHtml :: [FormatOption] -> SourceLine -> Html
sourceLineToHtml opts (SourceLine num contents) =
  concatHtml $ (thespan ! [theclass "LineNumber"] << (formatLineNumber num)) : (map (labeledSourceToHtml opts) contents) ++ [br]

formatLineNumber :: Int -> String
formatLineNumber = printf "%4d "

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')
