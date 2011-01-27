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
import Data.List (intersperse)

-- | Options for formatters.
data FormatOption = OptNumberLines     -- ^ Number lines
                  | OptNumberFrom Int  -- ^ Number of first line
                  | OptLineAnchors     -- ^ Anchors on each line number 
                  | OptTitleAttributes -- ^ Include title attributes
                  | OptDetailed        -- ^ Include detailed lexical information in classes.
                                       --   (By default, only the default style is included. This
                                       --   option causes output to be more verbose.)
                  deriving (Eq, Show, Read)

-- | Format a list of highlighted @SourceLine@s as XHtml.
formatAsXHtml :: [FormatOption]  -- ^ Options
              -> String          -- ^ Language
              -> [SourceLine]    -- ^ Source lines to format
              -> Html
formatAsXHtml opts lang lines =
  let startNum = getStartNum opts
      numberOfLines = length lines
      code = thecode << intersperse br (map (sourceLineToHtml opts) lines)
  in  if OptNumberLines `elem` opts
         then let lnTitle = title "Click to toggle line numbers"
                  lnOnClick = strAttr "onclick" "with (this.firstChild.style) { display = (display == '') ? 'none' : '' }"
                  nums = td ! [theclass "nums", lnTitle, lnOnClick] $ pre <<
                                     (intersperse br $ map lineNum [startNum..(startNum + numberOfLines - 1)])
                  lineNum n = if OptLineAnchors `elem` opts
                                 then anchor ! [identifier $ show n] << show n
                                 else stringToHtml $ show n
                  sourceCode = td ! [theclass "sourceCode"] $ 
                                    pre ! [theclass $ unwords ["sourceCode", lang]] $ code
              in  table ! [theclass "sourceCode"] $ tr << [nums, sourceCode]
         else pre ! [theclass $ unwords ["sourceCode", lang]] $ code

labeledSourceToHtml :: [FormatOption] -> LabeledSource -> Html
labeledSourceToHtml _ ([], txt)    = toHtml txt
labeledSourceToHtml opts (labs, txt)  =
  if null attribs
     then toHtml txt
     else thespan ! attribs << txt
   where classes = unwords $
                   if OptDetailed `elem` opts
                      then map removeSpaces labs
                      else drop 1 labs  -- first is specific
         attribs = [theclass classes | not (null classes)] ++
                   [title classes | OptTitleAttributes `elem` opts]

sourceLineToHtml :: [FormatOption] -> SourceLine -> Html
sourceLineToHtml opts contents =
  concatHtml $ map (labeledSourceToHtml opts) contents

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
  \pre.sourceCode span.kw { color: #007020; font-weight: bold; } \n\ 
  \pre.sourceCode span.dt { color: #902000; }\n\ 
  \pre.sourceCode span.dv { color: #40a070; }\n\ 
  \pre.sourceCode span.bn { color: #40a070; }\n\ 
  \pre.sourceCode span.fl { color: #40a070; }\n\ 
  \pre.sourceCode span.ch { color: #4070a0; }\n\ 
  \pre.sourceCode span.st { color: #4070a0; }\n\ 
  \pre.sourceCode span.co { color: #60a0b0; font-style: italic; }\n\ 
  \pre.sourceCode span.ot { color: #007020; }\n\ 
  \pre.sourceCode span.al { color: red; font-weight: bold; }\n\ 
  \pre.sourceCode span.fu { color: #06287e; }\n\ 
  \pre.sourceCode span.re { }\n\ 
  \pre.sourceCode span.er { color: red; font-weight: bold; }"
