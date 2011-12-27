{- |
   Module      : Text.Highlighting.Kate.Styles
   Copyright   : Copyright (C) 2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Styles for rendering annotated source lines.
-}

module Text.Highlighting.Kate.Styles ( pygments, kate, espresso, tango,
                                       haddock, monochrome )
where
import Text.Highlighting.Kate.Definitions

-- | Style based on pygments's default colors.
pygments :: Style
pygments = Style{
    backgroundColor = Nothing
  , defaultColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenColor = toColor "#007020", tokenBold = True })
    , (DataTypeTok, defStyle{ tokenColor = toColor "#902000" })
    , (DecValTok, defStyle{ tokenColor = toColor "#40a070" })
    , (BaseNTok, defStyle{ tokenColor = toColor "#40a070" })
    , (FloatTok, defStyle{ tokenColor = toColor "#40a070" })
    , (CharTok, defStyle{ tokenColor = toColor "#4070a0" })
    , (StringTok, defStyle{ tokenColor = toColor "#4070a0" })
    , (CommentTok, defStyle{ tokenColor = toColor "#60a0b0", tokenItalic = True })
    , (OtherTok, defStyle{ tokenColor = toColor "#007020" })
    , (AlertTok, defStyle{ tokenColor = toColor "#ff0000", tokenBold = True })
    , (FunctionTok, defStyle{ tokenColor = toColor "#06287e" })
    , (ErrorTok, defStyle{ tokenColor = toColor "#ff0000", tokenBold = True })
    ]
  }

-- | Style based on kate's default colors.
kate :: Style
kate = Style{
    backgroundColor = Nothing
  , defaultColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenBold = True })
    , (DataTypeTok, defStyle{ tokenColor = toColor "#800000" })
    , (DecValTok, defStyle{ tokenColor = toColor "#0000FF" })
    , (BaseNTok, defStyle{ tokenColor = toColor "#0000FF" })
    , (FloatTok, defStyle{ tokenColor = toColor "#800080" })
    , (CharTok, defStyle{ tokenColor = toColor "#FF00FF" })
    , (StringTok, defStyle{ tokenColor = toColor "#DD0000" })
    , (CommentTok, defStyle{ tokenColor = toColor "#808080", tokenItalic = True })
    , (AlertTok, defStyle{ tokenColor = toColor "#00ff00", tokenBold = True })
    , (FunctionTok, defStyle{ tokenColor = toColor "#000080" })
    , (ErrorTok, defStyle{ tokenColor = toColor "#ff0000", tokenBold = True })
    ]
  }

-- | Style based on pygments's tango colors.
tango :: Style
tango = Style{
    backgroundColor = toColor "#f8f8f8"
  , defaultColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenColor = toColor "#204a87", tokenBold = True })
    , (DataTypeTok, defStyle{ tokenColor = toColor "#204a87" })
    , (DecValTok, defStyle{ tokenColor = toColor "#0000cf" })
    , (BaseNTok, defStyle{ tokenColor = toColor "#0000cf" })
    , (FloatTok, defStyle{ tokenColor = toColor "#0000cf" })
    , (CharTok, defStyle{ tokenColor = toColor "#4e9a06" })
    , (StringTok, defStyle{ tokenColor = toColor "#4e9a06" })
    , (CommentTok, defStyle{ tokenColor = toColor "#8f5902", tokenItalic = True })
    , (OtherTok, defStyle{ tokenColor = toColor "#8f5902" })
    , (AlertTok, defStyle{ tokenColor = toColor "#ef2929" })
    , (FunctionTok, defStyle{ tokenColor = toColor "#000000" })
    , (ErrorTok, defStyle{ tokenColor = toColor "a40000", tokenBold = True })
    ]
  }

-- | Style based on ultraviolet's espresso_libre.css (dark background).
espresso :: Style
espresso = Style{
    backgroundColor = toColor "#2A211C"
  , defaultColor = toColor "#BDAE9D"
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenColor = toColor "#43A8ED", tokenBold = True })
    , (DataTypeTok, defStyle{ tokenUnderline = True })
    , (DecValTok, defStyle{ tokenColor = toColor "#44AA43" })
    , (BaseNTok, defStyle{ tokenColor = toColor "#44AA43" })
    , (FloatTok, defStyle{ tokenColor = toColor "#44AA43" })
    , (CharTok, defStyle{ tokenColor = toColor "#049B0A" })
    , (StringTok, defStyle{ tokenColor = toColor "#049B0A" })
    , (CommentTok, defStyle{ tokenColor = toColor "#0066FF", tokenItalic = True })
    , (AlertTok, defStyle{ tokenColor = toColor "#ffff00" })
    , (FunctionTok, defStyle{ tokenColor = toColor "#FF9358", tokenBold = True })
    , (ErrorTok, defStyle{ tokenColor = toColor "ffff00", tokenBold = True })
    ]
  }

-- | Style based on haddock's source highlighting.
haddock :: Style
haddock = Style{
    backgroundColor = Nothing
  , defaultColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenColor = toColor "#0000FF" })
    , (CharTok, defStyle{ tokenColor = toColor "#008080" })
    , (StringTok, defStyle{ tokenColor = toColor "#008080" })
    , (CommentTok, defStyle{ tokenColor = toColor "#008000" })
    , (OtherTok, defStyle{ tokenColor = toColor "#ff4000" })
    , (AlertTok, defStyle{ tokenColor = toColor "#ff0000" })
    , (ErrorTok, defStyle{ tokenColor = toColor "ff0000", tokenBold = True })
    ]
  }

-- | Style with no colors.
monochrome :: Style
monochrome = Style{
    backgroundColor = Nothing
  , defaultColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenBold = True })
    , (DataTypeTok, defStyle{ tokenUnderline = True })
    , (CommentTok, defStyle{ tokenItalic = True })
    , (AlertTok, defStyle{ tokenBold = True })
    , (ErrorTok, defStyle{ tokenBold = True })
    ]
  }
