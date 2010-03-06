{-# LANGUAGE CPP #-}
module Main where
import Text.Highlighting.Kate
import System.IO (hPutStrLn, stderr)
import System.Environment
import Text.XHtml.Transitional
import System.Console.GetOpt
import System.Exit
import System.FilePath (takeFileName, takeExtension)
import Data.Maybe (listToMaybe)
import Data.Char (toLower)

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

-- | Highlight source code in XHTML using specified syntax.
xhtmlHighlight :: [FormatOption] -- ^ Options
               -> String         -- ^ Name of syntax to use
               -> String         -- ^ Source code to highlight
               -> Html
xhtmlHighlight opts lang code =
  case highlightAs lang code of
       Right result -> formatAsXHtml opts lang result
       Left  _      -> pre $ thecode << code

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
                    Nothing  -> if null fnames
                                   then Nothing
                                   else let firstExt = drop 1 $ takeExtension $ head fnames
                                        in  listToMaybe $ languagesByExtension firstExt
  lang <- if lang' == Nothing
             then hPutStrLn stderr "No syntax specified." >>
                  hPutStrLn stderr (usageInfo usageHeader options) >>
                  exitWith (ExitFailure 5)
             else do let (Just l) = lang'
                     return (map toLower l)
  if not (lang `elem` (map (map toLower) languages))
     then hPutStrLn stderr ("Unknown syntax: " ++ lang) >> exitWith (ExitFailure 4)
     else return ()
  let highlightOpts = (if TitleAttributes `elem` opts then [OptTitleAttributes] else []) ++
                      (if NumberLines `elem` opts then [OptNumberLines, OptLineAnchors] else [])
  let css = case cssPathOf opts of
                   Nothing      -> style ! [thetype "text/css"] $ primHtml defaultHighlightingCss 
                   Just cssPath -> thelink ! [thetype "text/css", href cssPath, rel "stylesheet"] << noHtml
  let hcode = xhtmlHighlight highlightOpts lang code
  let pageTitle = if null fnames then noHtml else thetitle << (takeFileName $ head fnames)
  let metadata = meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"] +++
                 meta ! [name "generator", content "highlight-kate"]
  if Fragment `elem` opts
     then putStrLn $ renderHtmlFragment hcode
     else putStrLn $ renderHtml $ header << [pageTitle, metadata, css] +++ body << hcode
