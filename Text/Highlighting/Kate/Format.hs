{- |
   Module      : Text.Highlighting.Kate.Format
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Formatters that convert a list of annotated source lines to various output formats.
-}

module Text.Highlighting.Kate.Format ( formatAsXHtml, FormatOption (..) ) where
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
