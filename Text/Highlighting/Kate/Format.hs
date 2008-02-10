{- |
   Module      : Text.Highlighting.Kate.Format
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Formatters that convert a list of annotated source lines to various output formats.
-}

module Text.Highlighting.Kate.Format ( formatAsXHtml, FormatOption (..), defaultHighlightingCss ) where
import Text.Highlighting.Kate.Definitions
import Text.XHtml.Transitional

-- | Options for formatters.
data FormatOption = OptNumberLines     -- ^ Number lines
                  | OptNumberFrom Int  -- ^ Number of first line
                  | OptTitleAttributes -- ^ Include title attributes
                  deriving (Eq, Show, Read)

-- | Format a list of highlighted @SourceLine@s as XHtml.
formatAsXHtml :: [FormatOption]  -- ^ Options
              -> String          -- ^ Language
              -> [SourceLine]    -- ^ Source lines to format
              -> Html
formatAsXHtml opts lang lines =
  let startNum = getStartNum opts
      numberOfLines = length lines
      code = thecode << map (sourceLineToHtml opts) lines
  in  if OptNumberLines `elem` opts
         then let lnTitle = title "Click to toggle line numbers"
                  lnOnClick = strAttr "onclick" "with (this.firstChild.style) { display = (display == '') ? 'none' : '' }"
                  lineNumbers = td ! [theclass "lineNumbers", lnTitle, lnOnClick] $ pre <<
                                     (unlines $ map show [startNum..(startNum + numberOfLines - 1)])
                  sourceCode = td ! [theclass "sourceCode"] $ 
                                    pre ! [theclass $ unwords ["sourceCode", lang]] $ code
              in  table ! [theclass "sourceCode"] $ tr << [lineNumbers, sourceCode]
         else pre ! [theclass $ unwords ["sourceCode", lang]] $ code

labeledSourceToHtml :: [FormatOption] -> LabeledSource -> Html
labeledSourceToHtml opts (syntaxType, txt) =
  let classes = unwords $ filter (/= "") $ map removeSpaces syntaxType
  in if null classes 
        then toHtml txt
        else let attribs = [theclass classes] ++ 
                           if OptTitleAttributes `elem` opts
                              then [title classes] 
                              else []
             in  thespan ! attribs << txt 

sourceLineToHtml :: [FormatOption] -> SourceLine -> Html
sourceLineToHtml opts contents =
  concatHtml $ (map (labeledSourceToHtml opts) contents) ++ [br]

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

getStartNum :: [FormatOption] -> Int
getStartNum [] = 1
getStartNum (OptNumberFrom n : _) = n
getStartNum (_:xs) = getStartNum xs

defaultHighlightingCss :: String
defaultHighlightingCss = 
  "table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode, table.sourceCode pre \n\ 
  \   { margin: 0; padding: 0; border: 0; vertical-align: baseline; border: none; }\n\ 
  \td.lineNumbers { border-right: 1px solid #AAAAAA; text-align: right; color: #AAAAAA; padding-right: 5px; padding-left: 5px; }\n\  
  \td.sourceCode { padding-left: 5px; }\n\ 
  \pre.sourceCode { }\n\ 
  \pre.sourceCode span.Normal { }\n\ 
  \pre.sourceCode span.Keyword { color: #007020; font-weight: bold; } \n\ 
  \pre.sourceCode span.DataType { color: #902000; }\n\ 
  \pre.sourceCode span.DecVal { color: #40a070; }\n\ 
  \pre.sourceCode span.BaseN { color: #40a070; }\n\ 
  \pre.sourceCode span.Float { color: #40a070; }\n\ 
  \pre.sourceCode span.Char { color: #4070a0; }\n\ 
  \pre.sourceCode span.String { color: #4070a0; }\n\ 
  \pre.sourceCode span.Comment { color: #60a0b0; font-style: italic; }\n\ 
  \pre.sourceCode span.Others { color: #007020; }\n\ 
  \pre.sourceCode span.Alert { color: red; font-weight: bold; }\n\ 
  \pre.sourceCode span.Function { color: #06287e; }\n\ 
  \pre.sourceCode span.RegionMarker { }\n\ 
  \pre.sourceCode span.Error { color: red; font-weight: bold; }"
