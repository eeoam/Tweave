
\documentclass{article}%include polycode.fmt
\begin{document}

\section{}

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where


import Control.Arrow

import Data.Text as Text (Text, pack, append, empty, replace) 
import Data.Text.IO as TextIO (writeFile)
import Text.LaTeX as LaTeX
import Text.LaTeX.Base.Class (comm0)
--import Text.LaTeX.Packages
import Text.Parsec
import Text.Parsec.String
import Data.Functor.Identity
import Control.Monad.Writer

import System.FilePath
import System.Directory

import Data.List

import qualified Options.Applicative as Options

data Web = Web {preamble :: Text, sections :: Sections}

type Sections = [(Precis, Code)]

data Precis = Precis {title, body :: Text}

data Code = Code {file :: FilePath, name :: Text, fragments :: [Fragment]}
  deriving Eq
  
instance Ord Code where
  compare (Code _ nx _ ) (Code _ ny _) = compare nx ny
  
data Fragment = Kode Text | Link Text | 
  Dollar | Labrack | Rabrack | Lbrace | Rbrace | Larrow | Rarrow | BSlash | Prec
  deriving Eq

-- The @ symbol is a control character. If the programmer wants to use the actual @
-- symbol, they must type "@".
at_sign :: Parsec String FilePath Char
at_sign = char '@' >> char '@'


-- The preamble is all the characters up until the first @ followed by a space or '*'.
normal :: Parsec String FilePath Char
normal = noneOf ['@'] <|> try at_sign

preambl :: Parsec String FilePath Text
preambl = fmap pack $ many normal

precis :: Parsec String FilePath Precis
precis = do 
  char '@' 
  title <- precis_title
  ss <- many normal
  return $ Precis {Main.title = title, body = pack ss}

precis_title :: Parsec String FilePath Text
precis_title = 
  do{char ' '; return Text.empty} <|>
  do{char '*'; ss <- many (noneOf ['@','.'] <|> try at_sign); char '.'; return $ pack ss}







file_and_section_name :: Parsec String FilePath (FilePath, Text)
file_and_section_name = 
  do{char 'c'; path <- getState; return (get_hs_filename path, Text.empty)} <|>
  do{char '<'; ss <- many1 normal; char '@' >> char '>' >> char '='; path <- getState; 
     return (get_hs_filename path, Text.pack ss)} <|>
  do{char '('; ss <- many1 normal; char '@' >> char '>' >> char '='; return (ss, Text.empty)} 
  
code :: Parsec String FilePath Code
code = do
  char '@' 
  (path, name) <- file_and_section_name
  cc <- many1 (link_text <|> code_text <|> special)
  return $ Code {file = path, name = name, fragments = cc}
  
link_text :: Parsec String FilePath Fragment
link_text = do
  try $ char '@' >> char '<'
  ss <- many1 normal
  char '@' >> char '>'
  return $ Link $ pack ss
  
code_text :: Parsec String FilePath Fragment
code_text = do
  ss <- many1 code_normal
  return $ Kode $ pack ss

code_normal :: Parsec String FilePath Char
code_normal = noneOf ['@','$', '<','>', '{', '}', '-', '\\'] <|> try at_sign

special :: Parsec String FilePath Fragment
special = (char '$' >> return Dollar) <|>
  try (string "<-" >> return Larrow) <|>
  (char '<' >> return Labrack) <|>
  (char '>' >> return Rabrack) <|>
  try (string "->" >> return Rarrow) <|>
  try (string "~<" >> return Prec)  <|>
  (fmap (Kode . pack) $ string "-") <|>
  (char '{' >> return Lbrace) <|>
  (char '}' >> return Rbrace) <|>
  (char '\\' >> return BSlash) 

  
section :: Parsec String FilePath (Precis, Code)
section = do
  pp <- precis
  cc <- code
  return (pp, cc)
  

  

weave_section :: Monad m => (Precis, Code) -> LaTeXT m ()
weave_section (pp, cc) = weave_precis pp >> weave_code cc
  
weave_precis :: Monad m => Precis -> LaTeXT m ()
weave_precis ps = do
  if Main.title ps /= Text.empty 
  then raw "\\newpage" >> LaTeX.section (raw $ Main.title ps) >> (raw $ body ps)
  else LaTeX.subsection (raw $ Text.empty) >> (raw $ body ps)
  
  
weave_code :: Monad m => Code -> LaTeXT m ()
weave_code cc = do
  let escaped_file_name = concat $ map (\x -> if x == '\\' then "\\\\" else [x]) (file cc)
  let file_name = takeFileName $ file cc
  let code_title = if name cc == Text.empty then pack ("\\texttt{" ++ file_name ++ "}") else "$\\langle$\\relax{" `Text.append` name cc `Text.append` "}$\\rangle$"
  raw "\\subsection*{" 
  raw $ code_title
  raw "$\\ni:$}"
  raw "\n\\begin{Verbatim}[fontfamily=cmr, fontshape=it, commandchars=\\\\\\{\\}, codes={\\catcode `$=3}]\n"-- raw "\\begin{code}"
  foldr ((>>) . weave_fragment) (return ()) (fragments cc)
  raw "\n\\end{Verbatim}\n"
  --raw "\\end{code}"
  

weave_fragment :: Monad m => Fragment -> LaTeXT m ()
weave_fragment (Kode txt) = raw $ nicen txt
weave_fragment (Link txt) = do
  raw "$\\langle${\\upshape "
  raw txt 
  raw "}$\\rangle$"
weave_fragment Dollar = raw "\\$"
weave_fragment Labrack = raw "\\texttt{<}"
weave_fragment Rabrack = raw "\\texttt{>}"
weave_fragment Lbrace = raw "\\{"
weave_fragment Rbrace = raw "\\}"
weave_fragment Larrow = raw "$\\leftarrow$"
weave_fragment Rarrow = raw "$\\rightarrow$"
weave_fragment BSlash = raw "$\\backslash$"
weave_fragment Prec = raw "$\\prec$"

nicen :: Text -> Text
nicen = replace "data" "\\textbf{\\upshape data}" . replace "deriving" "\\textbf{\\upshape deriving}" . 
        replace "instance" "\\textbf{\\upshape instance}" . replace "=>" "$\\Rightarrow$" . replace "|" "$|$" .
        replace "import" "\\textbf{\\upshape{import}}" . replace "<-" "$\\leftarrow$" . 
        replace "->" "$\\rightarrow$" . replace "_" "\\texttt{_}" -- . replace "type" "\\textbf{\\upshape type}"



\end{code}


\section{}
\begin{code}
filename_ordering :: Code -> Code -> Ordering
filename_ordering cx cy = compare (file cx) (file cy)

unnamed_code :: Sections -> [Code]
unnamed_code ss = compress_code $ sortBy filename_ordering [cc|(_,cc) <- ss, name cc == Text.empty]

named_code :: Sections -> [Code]
named_code ss = compress_code $ sort [cc|(_,cc) <- ss, name cc /= Text.empty]

compress_code :: [Code] -> [Code]
compress_code [] = []
compress_code [cc] = [cc]
compress_code (cx:cy:xs) = 
  if (file cx, name cx) == (file cy, name cy)
    then compress_code $ merge_code cx cy : xs
    else cx : compress_code (cy : xs)

-- Precondition: f0 == f0 and n0 == n1
merge_code :: Code -> Code -> Code
merge_code (Code f0 n0 xs) (Code f1 n1 ys) = Code f0 n0 (xs ++ ys)

tangle_all :: [(Text, Code)] -> [Code] -> [Code]
tangle_all idx = map (tangle1 idx)

tangle1 :: [(Text, Code)] -> Code -> Code
tangle1 idx cx =
  let ys = concat $ map (visit_fragment idx) (fragments cx)
      cy = cx {fragments = ys}
  in if no_links ys then cy else tangle1 idx cy

--data Code = Code {file :: FilePath, name :: Text, fragments :: [Fragment]}
--data Fragment = Kode Text | Link Text
visit_fragment :: [(Text, Code)] -> Fragment -> [Fragment]
visit_fragment code_db (Kode txt) = [Kode txt]
visit_fragment code_db (Link txt) = case lookup txt code_db of
  Nothing -> fail $ "!Section name not defined: " ++ show txt
  Just cc -> fragments cc
visit_fragment code_db Dollar = [Dollar]
visit_fragment _ Labrack = [Labrack]
visit_fragment _ x = [x]

no_links :: [Fragment] -> Bool
no_links = all p where
  p (Kode txt) = True
  p (Link txt) = False
  p Dollar = True
  p _ = True

run_tangle :: Sections -> [Code]
run_tangle ss = 
  let 
    idx = mk_code_db $ named_code ss
  in tangle_all idx $ unnamed_code ss

mk_code_db :: [Code] -> [(Text, Code)]
mk_code_db xs = [(name cc, cc)| cc <- xs]

write_all_code :: [Code] -> IO()
write_all_code = mapM_ write_code

write_code :: Code -> IO()
write_code cx = do
  let dir = takeDirectory (file cx)
  createDirectoryIfMissing True dir
  TextIO.writeFile (file cx) (merge_fragments $ fragments cx)

-- precondition : all fragments are Kode fragments
merge_fragments :: [Fragment] -> Text
merge_fragments = foldr ((append) . f) Text.empty where 
  f (Kode txt) = txt
  f (Link lnk) = pack "This link was not expanded: " `append` lnk
  f Dollar = "$"
  f Labrack = "<"
  f Rabrack = ">"
  f Lbrace = "{"
  f Rbrace = "}"
  f Larrow = "<-"
  f Rarrow = "->"
  f BSlash = "\\"
  f Prec = "~<"

--------------------------------------------------------------
    
main :: IO() 
main = do
  putStrLn "This is Tweave, Version 2017-08-24 (amended 2020-03-25)"
  file_path <- Options.execParser (Options.info settings mempty)
  parse_web_file file_path
  putStrLn "done."

parse_web_file :: FilePath -> IO()
parse_web_file ss = do
  let path = mk_tweave_filename ss
  putStr $ "Tweaving " ++ path ++ "..."
  readFile path >>= parse_web path

parse_web :: FilePath -> String -> IO()
parse_web path ss = case runParser web path "" ss of
  Left err -> print err
  Right (preamble, sections) -> do 
    let texput = foldr ((>>) . weave_section) (return()) sections
    let prep = raw "%preamble\n"
    execLaTeXT (prep <> texput) >>= renderFile (get_lhs_filename path)
    write_all_code . run_tangle $ sections

web :: Parsec String FilePath (Text, [(Precis, Code)])
web = do 
  pe <- preambl 
  ps <- many Main.section
  return (pe, ps)

doc :: Monad m => (Text, [(Precis, Code)]) -> LaTeXT m ()
doc (pe, secs) = do
  documentclass [] article 
  raw "\\usepackage{beamerarticle}"
  comment "\\documentclass[ignoreonframetext]{beamer}"
  raw "\\mode<article>{\\usepackage{fullpage}}"
  raw pe
  raw "\\usepackage{fancyvrb}"
  raw "\\usepackage{amssymb}"
  raw "\\usepackage{imakeidx}"
  raw "\\makeindex"
  document $ do
    raw "\\tableofcontents"
    foldr ((>>) . weave_section) (return()) secs
    raw "\\printindex"

settings :: Options.Parser FilePath
settings = Options.argument Options.str $ Options.metavar "TWEAVEFILE"

get_hs_filename = (++ ".hs") . get_filename_stem
get_lhs_filename = (++ ".tex") . get_filename_stem
get_filename_stem = reverse . tail . dropWhile (/= '.') . reverse


mk_tweave_filename :: FilePath -> FilePath
mk_tweave_filename ss = let 
  (name, suffix) = swap . (reverse *** init . reverse) . span (/= '.') . reverse $ ss
  filename = if suffix == "tweave" then ss else ss ++ ".tweave"
  in filename

swap (x,y) = (y,x) 


\end{code}

\end{document}

