{- |
   Module      : Text.Highlighting.Kate.Format
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Formatters that convert a list of annotated source lines to various output formats.
-}

module Text.Highlighting.Kate.Format ( formatAsHtml, formatAsLaTeX, FormatOption (..), defaultHighlightingCss, defaultLaTeXMacros ) where
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


--
-- LaTeX
--

-- | Format a list of highlighted @SourceLine@s as LaTeX.
formatAsLaTeX :: [FormatOption]  -- ^ Options
              -> [SourceLine]    -- ^ Source lines to format
              -> String
formatAsLaTeX opts lines =
  let startNum = getStartNum opts
      code = unlines $ map sourceLineToLaTeX lines
      commandchars = "commandchars=\\\\\\{\\}"
  in  if OptInline `elem` opts
         then "\\Verb[" ++ commandchars ++ "]{" ++ code ++ "}"
         else "\\begin{Verbatim}[" ++
              (if OptNumberLines `elem` opts
                  then "numbers=left," ++
                       (if startNum == 1
                           then ""
                           else ",firstnumber=" ++ show startNum) ++ ","
                  else ""
              ) ++ commandchars ++ "]\n" ++ code ++ "\\end{Verbatim}"

tokenToLaTeX :: Token -> String
tokenToLaTeX (NormalTok, txt) = escapeLaTeX txt
tokenToLaTeX (toktype, txt)   = '\\':(short toktype ++ "{" ++ escapeLaTeX txt ++ "}")

escapeLaTeX :: String -> String
escapeLaTeX = concatMap escapeLaTeXChar
  where escapeLaTeXChar '\\' = "\\textbackslash{}"
        escapeLaTeXChar '{'  = "\\{"
        escapeLaTeXChar '}'  = "\\}"
        escapeLaTeXChar x    = [x]

sourceLineToLaTeX :: SourceLine -> String
sourceLineToLaTeX contents = concatMap tokenToLaTeX contents

defaultLaTeXMacros :: String
defaultLaTeXMacros =
  "\\usepackage{color}\n\
  \\\usepackage{fancyvrb}\n\
  \\\definecolor{kwcolor}{RGB}{0,112,32}\n\
  \\\definecolor{dtcolor}{RGB}{144,32,0}\n\
  \\\definecolor{dvcolor}{RGB}{64,160,112}\n\
  \\\definecolor{bncolor}{RGB}{64,160,112}\n\
  \\\definecolor{flcolor}{RGB}{64,160,112}\n\
  \\\definecolor{chcolor}{RGB}{64,112,160}\n\
  \\\definecolor{stcolor}{RGB}{64,112,160}\n\
  \\\definecolor{cocolor}{RGB}{96,160,176}\n\
  \\\definecolor{otcolor}{RGB}{0,112,32}\n\
  \\\definecolor{alcolor}{RGB}{255,0,0}\n\
  \\\definecolor{fucolor}{RGB}{6,40,126}\n\
  \\\definecolor{ercolor}{RGB}{255,0,0}\n\
  \\\newcommand{\\kw}[1]{\\textcolor{kwcolor}{#1}}\n\
  \\\newcommand{\\dt}[1]{\\textcolor{dtcolor}{#1}}\n\
  \\\newcommand{\\dv}[1]{\\textcolor{dvcolor}{#1}}\n\
  \\\newcommand{\\bn}[1]{\\textcolor{bncolor}{#1}}\n\
  \\\newcommand{\\fl}[1]{\\textcolor{flcolor}{#1}}\n\
  \\\newcommand{\\ch}[1]{\\textcolor{chcolor}{#1}}\n\
  \\\newcommand{\\st}[1]{\\textcolor{stcolor}{#1}}\n\
  \\\newcommand{\\co}[1]{\\textcolor{cocolor}{\\textit{#1}}}\n\
  \\\newcommand{\\ot}[1]{\\textcolor{otcolor}{#1}}\n\
  \\\newcommand{\\al}[1]{\\textcolor{cocolor}{\\textbf{#1}}}\n\
  \\\newcommand{\\fu}[1]{\\textcolor{fucolor}{#1}}\n\
  \\\newcommand{\\re}[1]{#1}\n\
  \\\newcommand{\\er}[1]{\\textcolor{ercolor}{\\textbf{#1}}}\n"

--
-- HTML
--

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
                      in  H.table ! A.class_ (toValue "sourceCode") $ H.tr ! A.class_ (toValue "sourceCode") $ nums >> sourceCode
                 else H.pre ! A.class_ (toValue "sourceCode") $ code

tokenToHtml :: [FormatOption] -> Token -> Html
tokenToHtml _ (NormalTok, txt)    = toHtml txt
tokenToHtml opts (toktype, txt)  =
  titleize $ H.span ! A.class_ (toValue $ short toktype) $ toHtml txt
    where titleize x = if OptTitleAttributes `elem` opts
                          then x ! A.title (toValue $ show toktype)
                          else x

short :: TokenType -> String
short KeywordTok        = "kw"
short DataTypeTok       = "dt"
short DecValTok         = "dv"
short BaseNTok          = "bn"
short FloatTok          = "fl"
short CharTok           = "ch"
short StringTok         = "st"
short CommentTok        = "co"
short OtherTok          = "ot"
short AlertTok          = "al"
short FunctionTok       = "fu"
short RegionMarkerTok   = "re"
short ErrorTok          = "er"
short NormalTok         = ""

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
