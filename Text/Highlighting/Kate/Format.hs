{- |
   Module      : Text.Highlighting.Kate.Format
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Formatters that convert a list of annotated source lines to various output formats.
-}

module Text.Highlighting.Kate.Format ( formatAsHtml, FormatOption (..), defaultHighlightingCss ) where
import Text.Highlighting.Kate.Definitions
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid
import Data.List (intersperse)

-- | Options for formatters.
data FormatOption = OptNumberLines     -- ^ Number lines
                  | OptNumberFrom Int  -- ^ Number of first line
                  | OptLineAnchors     -- ^ Anchors on each line number
                  | OptTitleAttributes -- ^ Include title attributes
                  | OptInline          -- ^ Format as span-level, not block-level element
                  deriving (Eq, Show, Read)

-- | Format a list of highlighted @SourceLine@s as Html.
formatAsHtml :: [FormatOption]  -- ^ Options
              -> String          -- ^ Language
              -> [SourceLine]    -- ^ Source lines to format
              -> Html
formatAsHtml opts lang lines =
  let startNum = getStartNum opts
      numberOfLines = length lines
      code = H.code ! A.class_ (toValue $ unwords ["sourceCode", lang])
                    $ mconcat $ intersperse (toHtml "\n") $ map (sourceLineToHtml opts) lines
  in  if OptInline `elem` opts
         then code
         else if OptNumberLines `elem` opts
                 then let lnTitle = A.title (toValue "Click to toggle line numbers")
                          lnOnClick = A.onclick $ toValue
                                                $ "with (this.firstChild.style) { display = (display == '') ? 'none' : '' }"
                          nums = H.td ! A.class_ (toValue "lineNumbers") ! lnTitle ! lnOnClick
                                      $ H.pre
                                      $ mapM_ lineNum [startNum..(startNum + numberOfLines - 1)]
                          lineNum n = if OptLineAnchors `elem` opts
                                         then (H.a ! A.id (toValue $ show n) $ toHtml $ show n)
                                               >> toHtml "\n"
                                         else toHtml $ show n ++ "\n"
                          sourceCode = H.td ! A.class_ (toValue "sourceCode")
                                            $ H.pre ! A.class_ (toValue "sourceCode") $ code
                      in  H.table ! A.class_ (toValue "sourceCode") $ H.tr $ nums >> sourceCode
                 else H.pre ! A.class_ (toValue "sourceCode") $ code

tokenToHtml :: [FormatOption] -> Token -> Html
tokenToHtml _ ([], txt)    = toHtml txt
tokenToHtml opts (lab, txt)  =
  if null lab
     then toHtml txt
     else titleize $ H.span ! A.class_ (toValue lab) $ toHtml txt
   where titleize x = if OptTitleAttributes `elem` opts
                         then x ! A.title (toValue lab)
                         else x

sourceLineToHtml :: [FormatOption] -> SourceLine -> Html
sourceLineToHtml opts contents = mapM_ (tokenToHtml opts) contents

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
  \code.sourceCode span.kw { color: #007020; font-weight: bold; } \n\
  \code.sourceCode span.dt { color: #902000; }\n\
  \code.sourceCode span.dv { color: #40a070; }\n\
  \code.sourceCode span.bn { color: #40a070; }\n\
  \code.sourceCode span.fl { color: #40a070; }\n\
  \code.sourceCode span.ch { color: #4070a0; }\n\
  \code.sourceCode span.st { color: #4070a0; }\n\
  \code.sourceCode span.co { color: #60a0b0; font-style: italic; }\n\
  \code.sourceCode span.ot { color: #007020; }\n\
  \code.sourceCode span.al { color: red; font-weight: bold; }\n\
  \code.sourceCode span.fu { color: #06287e; }\n\
  \code.sourceCode span.re { }\n\
  \code.sourceCode span.er { color: red; font-weight: bold; }"
