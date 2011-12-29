{- |
   Module      : Text.Highlighting.Kate.Format
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Formatters that convert a list of annotated source lines to various output
formats.
-}

module Text.Highlighting.Kate.Format (
         FormatOptions(..), defaultFormatOpts,
         formatHtmlInline, formatHtmlBlock, styleToHtml,
         formatLaTeXInline, formatLaTeXBlock, styleToLaTeX
         ) where
import Text.Highlighting.Kate.Definitions
import Text.Blaze
import Text.Printf
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid
import Data.List (intersperse, intercalate)
import Control.Monad (mplus)
import Data.Char (isSpace)

-- | Options for formatting source code.
data FormatOptions = FormatOptions{
         numberLines      :: Bool     -- ^ Number lines
       , startNumber      :: Int      -- ^ Number of first line
       , lineAnchors      :: Bool     -- ^ Anchors on each line number
       , titleAttributes  :: Bool     -- ^ Html titles with token types
       , codeClasses      :: [String] -- ^ Additional classes for Html code tag
       , containerClasses :: [String] -- ^ Additional classes for Html container tag
                                      --   (pre or table depending on numberLines)
       } deriving (Eq, Show, Read)

defaultFormatOpts :: FormatOptions
defaultFormatOpts = FormatOptions{
                      numberLines = False
                    , startNumber = 1
                    , lineAnchors = False
                    , titleAttributes = False
                    , codeClasses = []
                    , containerClasses = []
                    }

-- | Format tokens using HTML spans inside @code@ tags. For example,
-- A @KeywordTok@ is rendered as a span with class @kw@.
-- Short class names correspond to 'TokenType's as follows:
-- 'KeywordTok' = @kw@, 'DataTypeTok' = @dt@,
-- 'DecValTok' = @dv@, 'BaseNTok' = @bn@, 'FloatTok' = @fl@,
-- 'CharTok' = @ch@, 'StringTok' = @st@, 'CommontTok' = @co@,
-- 'OtherTok' = @ot@, 'AlertTok' = @al@, 'FunctionTok' = @fu@,
-- 'RegionMarkerTok' = @re@, 'ErrorTok' = @er@. A 'NormalTok'
-- is not marked up at all.
formatHtmlInline :: FormatOptions -> [SourceLine] -> Html
formatHtmlInline opts = (H.code ! A.class_ (toValue $ unwords
                                                    $ "sourceCode" : codeClasses opts))
                                . mconcat . intersperse (toHtml "\n")
                                . map (sourceLineToHtml opts)

tokenToHtml :: FormatOptions -> Token -> Html
tokenToHtml _ (NormalTok, txt)  = toHtml txt
tokenToHtml opts (toktype, txt) =
  if titleAttributes opts
     then sp ! A.title (toValue $ show toktype)
     else sp
   where sp = H.span ! A.class_ (toValue $ short toktype) $ toHtml txt

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

sourceLineToHtml :: FormatOptions -> SourceLine -> Html
sourceLineToHtml opts contents = mapM_ (tokenToHtml opts) contents

formatHtmlBlockPre :: FormatOptions -> [SourceLine] -> Html
formatHtmlBlockPre opts = H.pre . formatHtmlInline opts

-- | Format tokens as an HTML @pre@ block. If line numbering is
-- selected, this is put into a table row with line numbers in the
-- left cell.
formatHtmlBlock :: FormatOptions -> [SourceLine] -> Html
formatHtmlBlock opts ls = container ! A.class_ (toValue $ unwords classes)
  where  container = if numberLines opts
                        then H.table $ H.tr ! A.class_ sourceCode
                                     $ nums >> source
                        else pre
         sourceCode = toValue "sourceCode"
         classes = "sourceCode" :
                   [x | x <- containerClasses opts, x /= "sourceCode"]
         pre = formatHtmlBlockPre opts ls
         source = H.td ! A.class_ sourceCode $ pre
         startNum = startNumber opts
         nums = H.td ! A.class_ (toValue "lineNumbers")
                     $ H.pre
                     $ mapM_ lineNum [startNum..(startNum + length ls - 1)]
         lineNum n = if lineAnchors opts
                        then (H.a ! A.id (toValue $ show n) $ toHtml $ show n)
                              >> toHtml "\n"
                        else toHtml $ show n ++ "\n"

-- | Returns an HTML @style@ block with definitions for
-- formatting highlighted code according to the given style.
styleToHtml :: Style -> Html
styleToHtml f = H.style ! A.type_ (toValue "text/css") $ toHtml
  $ unlines $ tablespec ++ colorspec ++ map toCss (tokenStyles f)
   where colorspec = case (defaultColor f, backgroundColor f) of
                          (Nothing, Nothing) -> []
                          (Just c, Nothing)  -> ["pre, code { color: " ++ fromColor c ++ "; }"]
                          (Nothing, Just c)  -> ["pre, code { background-color: " ++ fromColor c ++ "; }"]
                          (Just c1, Just c2) -> ["pre, code { color: " ++ fromColor c1 ++ "; background-color: " ++
                                                  fromColor c2 ++ "; }"]
         tablespec = [
           "table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {"
          ,"  margin: 0; padding: 0; vertical-align: baseline; border: none; }"
          ,"table.sourceCode { " ++
             maybe "" (\c -> "background-color: " ++ fromColor c ++ "; ") (backgroundColor f) ++
             maybe "" (\c -> "color: " ++ fromColor c ++ "; ") (defaultColor f) ++
             "}"
          ,"td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; " ++
             maybe "" (\c -> "background-color: " ++ fromColor c ++ "; ") (lineNumberBackgroundColor f) ++
             maybe "" (\c -> "color: " ++ fromColor c ++ "; ") (lineNumberColor f) ++
             maybe "" (\c -> "border-right: 1px solid " ++ fromColor c ++ "; ") (lineNumberColor f) ++
             "}"
          ,"td.sourceCode { padding-left: 5px; }"
          ]

toCss :: (TokenType, TokenStyle) -> String
toCss (t,tf) = "code > span." ++ short t ++ " { "
                ++ colorspec ++ backgroundspec ++ weightspec ++ stylespec
                ++ decorationspec ++ "}"
  where colorspec = maybe "" (\col -> "color: " ++ fromColor col ++ "; ") $ tokenColor tf
        backgroundspec = maybe "" (\col -> "background-color: " ++ fromColor col ++ "; ") $ tokenBackground tf
        weightspec = if tokenBold tf then "font-weight: bold; " else ""
        stylespec  = if tokenItalic tf then "font-style: italic; " else ""
        decorationspec = if tokenUnderline tf then "text-decoration: underline; " else ""

formatLaTeX :: FormatOptions -> [SourceLine] -> String
formatLaTeX _ = intercalate "\n" . map sourceLineToLaTeX

-- | Formats tokens as LaTeX using custom commands inside
-- a @\\Highlight@ command.  A @KeywordTok@ is rendered
-- using @\\KeywordTok{..}@, and so on.
formatLaTeXInline :: FormatOptions -> [SourceLine] -> String
formatLaTeXInline opts ls = "\\Verb{" ++ formatLaTeX opts ls ++ "}"

sourceLineToLaTeX :: SourceLine -> String
sourceLineToLaTeX contents = concatMap tokenToLaTeX contents

tokenToLaTeX :: Token -> String
tokenToLaTeX (NormalTok, txt) | all isSpace txt = escapeLaTeX txt
tokenToLaTeX (toktype, txt)   = '\\':(show toktype ++ "{" ++ escapeLaTeX txt ++ "}")

escapeLaTeX :: String -> String
escapeLaTeX = concatMap escapeLaTeXChar
  where escapeLaTeXChar '\\' = "\\textbackslash{}"
        escapeLaTeXChar '{'  = "\\{"
        escapeLaTeXChar '}'  = "\\}"
        escapeLaTeXChar '|'  = "\\textbar{}" -- used in inline verbatim
        escapeLaTeXChar x    = [x]

-- LaTeX

-- | Format tokens as a LaTeX @Highlighting@ environment inside a
-- @Shaded@ environment.  @Highlighting@ and @Shaded@ are
-- defined by the macros produced by 'styleToLaTeX'.  @Highlighting@
-- is a verbatim environment using @fancyvrb@; @\\@, @{@, and @}@
-- have their normal meanings inside this environment, so that
-- formatting commands work.  @Shaded@ is either nothing
-- (if the style's background color is default) or a @snugshade@
-- environment from @framed@, providing a background color
-- for the whole code block, even if it spans multiple pages.
formatLaTeXBlock :: FormatOptions -> [SourceLine] -> String
formatLaTeXBlock opts ls = unlines
  ["\\begin{Shaded}"
  ,"\\begin{Highlighting}[" ++
   (if numberLines opts
       then "numbers=left," ++
            (if startNumber opts == 1
                then ""
                else ",firstnumber=" ++ show (startNumber opts)) ++ ","
       else "") ++ "]"
  ,formatLaTeX opts ls
  ,"\\end{Highlighting}"
  ,"\\end{Shaded}"]

-- | Converts a 'Style' to a set of LaTeX macro definitions,
-- which should be placed in the document's preamble.
-- Note: default LaTeX setup doesn't allow boldface typewriter font.
-- To make boldface work in styles, you need to use a different typewriter
-- font. This will work for computer modern:
--
-- > \DeclareFontShape{OT1}{cmtt}{bx}{n}{<5><6><7><8><9><10><10.95><12><14.4><17.28><20.74><24.88>cmttb10}{}
--
-- Or, with xelatex:
--
-- > \usepackage{fontspec}
-- > \setmainfont[SmallCapsFont={* Caps}]{Latin Modern Roman}
-- > \setsansfont{Latin Modern Sans}
-- > \setmonofont[SmallCapsFont={Latin Modern Mono Caps}]{Latin Modern Mono Light}
--
styleToLaTeX :: Style -> String
styleToLaTeX f = unlines $
  [ "\\usepackage{color}"
  , "\\usepackage{fancyvrb}"
  , "\\DefineShortVerb[commandchars=\\\\\\{\\}]{\\|}"
  , "\\DefineVerbatimEnvironment{Highlighting}{Verbatim}{commandchars=\\\\\\{\\}}"
  , "% Add ',fontsize=\\small' for more characters per line"
  ] ++
  (case backgroundColor f of
        Nothing          -> ["\\newenvironment{Shaded}{}{}"]
        Just (RGB r g b) -> ["\\usepackage{framed}"
                            ,printf "\\definecolor{shadecolor}{RGB}{%d,%d,%d}" r g b
                            ,"\\newenvironment{Shaded}{\\begin{snugshade}}{\\end{snugshade}}"])
  ++ map (macrodef (defaultColor f) (tokenStyles f)) (enumFromTo KeywordTok NormalTok)

macrodef :: Maybe Color -> [(TokenType, TokenStyle)] -> TokenType -> String
macrodef defaultcol tokstyles tokt = "\\newcommand{\\" ++ show tokt ++
                     "}[1]{" ++ (co . ul . bf . it . bg $ "{#1}") ++ "}"
  where tokf = case lookup tokt tokstyles of
                     Nothing -> defStyle
                     Just x  -> x
        ul x = if tokenUnderline tokf
                  then "\\underline{" ++ x ++ "}"
                  else x
        it x = if tokenItalic tokf
                  then "\\textit{" ++ x ++ "}"
                  else x
        bf x = if tokenBold tokf
                  then "\\textbf{" ++ x ++ "}"
                  else x
        bcol = fromColor `fmap` tokenBackground tokf :: Maybe (Double, Double, Double)
        bg x = case bcol of
                    Nothing          -> x
                    Just (r, g, b) -> printf "\\colorbox[rgb]{%0.2f,%0.2f,%0.2f}{%s}" r g b x
        col  = fromColor `fmap`
                 (tokenColor tokf `mplus` defaultcol) :: Maybe (Double, Double, Double)
        co x = case col of
                    Nothing        -> x
                    Just (r, g, b) -> printf "\\textcolor[rgb]{%0.2f,%0.2f,%0.2f}{%s}" r g b x

-- formatConTeXtBlock :: FormatOptions -> String -> String
-- formatConTeXtBlock = undefined

-- styleToConTeXt :: Style -> String
-- styleToConTeXt = undefined
