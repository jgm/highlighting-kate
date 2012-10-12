{- |
   Module      : Text.Highlighting.Kate.Format.LaTeX
   Copyright   : Copyright (C) 2008-2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Formatters that convert a list of annotated source lines to LaTeX.
-}

module Text.Highlighting.Kate.Format.LaTeX (
         formatLaTeXInline, formatLaTeXBlock, styleToLaTeX
         ) where
import Text.Highlighting.Kate.Types
import Text.Printf
import Data.List (intercalate)
import Control.Monad (mplus)
import Data.Char (isSpace)

formatLaTeX :: FormatOptions -> [SourceLine] -> String
formatLaTeX _ = intercalate "\n" . map sourceLineToLaTeX

-- | Formats tokens as LaTeX using custom commands inside
-- @|@ characters. Assumes that @|@ is defined as a short verbatim
-- command by the macros produced by 'styleToLaTeX'.
-- A @KeywordTok@ is rendered using @\\KeywordTok{..}@, and so on.
formatLaTeXInline :: FormatOptions -> [SourceLine] -> String
formatLaTeXInline opts ls = "|" ++ formatLaTeX opts ls ++ "|"

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
        escapeLaTeXChar '`'  = "{\\char18}"  -- otherwise will be curly
        escapeLaTeXChar '\'' = "{\\char13}"  -- otherwise will be curly
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

