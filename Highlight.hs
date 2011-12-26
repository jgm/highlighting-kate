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

data Flag = Sty String
          | Format String
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
  [ Option ['S'] ["style"] (ReqArg Sty "STYLE") "specify style"
  , Option ['F'] ["format"] (ReqArg Format "FORMAT")  "output format (html|latex)"
  , Option ['f'] ["fragment"] (NoArg Fragment)  "fragment, without document header"
  , Option ['h'] ["help"] (NoArg Help)   "show usage message"
  , Option ['l'] ["list"] (NoArg List)   "list available language syntaxes"
  , Option ['n'] ["number-lines"] (NoArg NumberLines)  "number lines"
  , Option ['s'] ["syntax"] (ReqArg Syntax "SYNTAX")  "specify language syntax to use"
  , Option ['t'] ["title-attributes"] (NoArg TitleAttributes)  "include structure in title attributes"
  , Option ['v'] ["version"] (NoArg Version)   "print version"
  ]

syntaxOf :: [Flag] -> Maybe String
syntaxOf [] = Nothing
syntaxOf (Syntax s : _) = Just s
syntaxOf (_:xs) = syntaxOf xs

styleOf :: [Flag] -> Maybe Style
styleOf [] = Nothing
styleOf (Sty s : _) = case map toLower s of
                            "pygments"  -> Just pygments
                            "espresso"  -> Just espresso
                            "kate"      -> Just kate
                            "tango"     -> Just tango
                            _           -> error $ "Unknown style: " ++ s
styleOf (_ : xs) = styleOf xs

formatOf :: [Flag] -> String
formatOf [] = "html" -- default
formatOf (Format s : _) = case map toLower s of
                            "html"   -> "html"
                            "latex"  -> "latex"
                            _        -> error $ "Unknown format: " ++ s
formatOf (_ : xs) = formatOf xs

filterNewlines :: String -> String
filterNewlines ('\r':'\n':xs) = '\n' : filterNewlines xs
filterNewlines ('\r':xs) = '\n' : filterNewlines xs
filterNewlines (x:xs) = x : filterNewlines xs
filterNewlines [] = []

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
  let highlightOpts = defaultFormatOpts{ titleAttributes = TitleAttributes `elem` opts
                                       , numberLines = NumberLines `elem` opts
                                       , lineAnchors = NumberLines `elem` opts
                                       }
  let fragment = Fragment `elem` opts
  let fname = case fnames of
                    []    -> ""
                    (x:_) -> x
  case formatOf opts of
       "html"  -> hlHtml fragment fname highlightOpts (maybe pygments id $ styleOf opts)
                       lang code
       "latex" -> hlLaTeX fragment fname highlightOpts (maybe pygments id $ styleOf opts) lang code
       x       -> error $ "Uknown format " ++  x

hlHtml :: Bool               -- ^ Fragment
      -> FilePath            -- ^ Filename
      -> FormatOptions
      -> Style
      -> String              -- ^ language
      -> String              -- ^ code
      -> IO ()
hlHtml frag fname opts sty lang code =
 if frag
    then putStrLn $ renderHtml fragment
    else putStrLn $ renderHtml $ H.head (pageTitle >> metadata >> css) >> H.body (toHtml fragment)
  where fragment = formatHtmlBlock opts $ highlightAs lang code
        css = styleToHtml sty
        pageTitle = H.title $ toHtml fname
        metadata = H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8" >>
                    H.meta ! A.name "generator" ! A.content "highlight-kate"

hlLaTeX :: Bool               -- ^ Fragment
        -> FilePath            -- ^ Filename
        -> FormatOptions
        -> Style
        -> String              -- ^ language
        -> String              -- ^ code
        -> IO ()
hlLaTeX frag fname opts sty lang code =
 if frag
    then putStrLn fragment
    else putStrLn $ "\\documentclass{article}\n\\usepackage[margin=1in]{geometry}\n" ++
                    macros ++ pageTitle ++
                    "\n\\begin{document}\n\\maketitle\n" ++  fragment ++ "\n\\end{document}"
  where fragment = formatLaTeXBlock opts $ highlightAs lang code
        macros = styleToLaTeX sty
        pageTitle = "\\title{" ++ fname ++ "}\n"



