{-# LANGUAGE CPP, OverloadedStrings #-}
module Main where
import Text.Highlighting.Kate
import System.IO (hPutStrLn, stderr)
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.FilePath (takeFileName)
import Data.Maybe (listToMaybe)
import Data.Char (toLower)
import Text.Blaze.Renderer.String
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Flag = CssPath String
          | Help
          | Fragment
          | List
          | NumberLines
          | Syntax String
          | TitleAttributes
          | Version
          deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['c'] ["css"] (ReqArg CssPath "PATH") "link CSS file"
  , Option ['f'] ["fragment"] (NoArg Fragment)  "fragment, without document header"
  , Option ['h'] ["help"] (NoArg Help)   "show usage message"
  , Option ['l'] ["list"] (NoArg List)   "list available language syntaxes"
  , Option ['n'] ["number-lines"] (NoArg NumberLines)  "number lines"
  , Option ['s'] ["syntax"] (ReqArg Syntax "SYNTAX")  "specify language syntax to use"
  , Option ['t'] ["title-attributes"] (NoArg TitleAttributes)  "include structure in title attributes"
  , Option ['v'] ["version"] (NoArg Version)   "print version"
  ]

cssPathOf :: [Flag] -> Maybe String
cssPathOf [] = Nothing
cssPathOf (CssPath s : _) = Just s
cssPathOf (_:xs) = cssPathOf xs

syntaxOf :: [Flag] -> Maybe String
syntaxOf [] = Nothing
syntaxOf (Syntax s : _) = Just s
syntaxOf (_:xs) = syntaxOf xs

filterNewlines :: String -> String
filterNewlines ('\r':'\n':xs) = '\n' : filterNewlines xs
filterNewlines ('\r':xs) = '\n' : filterNewlines xs
filterNewlines (x:xs) = x : filterNewlines xs
filterNewlines [] = []

-- | Highlight source code in HTML using specified syntax.
xhtmlHighlight :: [FormatOption] -- ^ Options
               -> String         -- ^ Name of syntax to use
               -> String         -- ^ Source code to highlight
               -> Html
xhtmlHighlight opts lang code =
  case highlightAs lang code of
       Right result -> formatAsHtml opts lang result
       Left  _      -> H.pre $ H.code $ toHtml code

main = do
  (opts, fnames, errs) <- getArgs >>= return . getOpt Permute options
  prg <- getProgName
  let usageHeader = prg ++ " [options] [files...]"
  if not (null errs)
     then ioError (userError $ concat errs ++ usageInfo usageHeader options)
     else return ()
  if List `elem` opts
     then putStrLn (unwords languages) >> exitWith ExitSuccess
     else return ()
  if Help `elem` opts
     then hPutStrLn stderr (usageInfo usageHeader options) >> 
          exitWith (ExitFailure 1)
     else return ()
  if Version `elem` opts
     then putStrLn (prg ++ " " ++ highlightingKateVersion ++ " - (c) 2008 John MacFarlane") >> 
          exitWith ExitSuccess
     else return ()
  code <- if null fnames
             then getContents >>= return . filterNewlines
             else mapM readFile fnames >>= return . filterNewlines . concat
  let lang' = case syntaxOf opts of
                    Just e   -> Just e
                    Nothing  -> case fnames of
                                     []     -> Nothing
                                     (x:_)  -> listToMaybe $ languagesByFilename $ takeFileName x
  lang <- if lang' == Nothing
             then hPutStrLn stderr "No syntax specified." >>
                  hPutStrLn stderr (usageInfo usageHeader options) >>
                  exitWith (ExitFailure 5)
             else do let (Just l) = lang'
                     return (map toLower l)
  if not (lang `elem` (map (map toLower) languages))
     then hPutStrLn stderr ("Unknown syntax: " ++ lang) >> exitWith (ExitFailure 4)
     else return ()
  let highlightOpts = [OptTitleAttributes | TitleAttributes `elem` opts] ++
                      [OptNumberLines | NumberLines `elem` opts] ++
                      [OptLineAnchors | NumberLines `elem` opts]
  let css = case cssPathOf opts of
                   Nothing      -> H.style ! A.type_ "text/css" $ toHtml defaultHighlightingCss
                   Just cssPath -> H.link ! A.type_ "text/css" ! A.href (toValue cssPath) ! A.rel "stylesheet"
  let hcode = xhtmlHighlight highlightOpts lang code
  let pageTitle = if null fnames then return () else H.title $ (toHtml $ takeFileName $ head fnames)
  let metadata = H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8" >>
                 H.meta ! A.name "generator" ! A.content "highlight-kate"
  if Fragment `elem` opts
     then putStrLn $ renderHtml hcode
     else putStrLn $ renderHtml $ H.head (pageTitle >> metadata >> css) >> H.body (toHtml hcode)
