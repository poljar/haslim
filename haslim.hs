
module Main where

import Control.Monad.State
import Text.Parsec hiding (State, parse)
import Text.Parsec.Indent

import Data.Maybe

import Data.String.Utils (strip)

type Parser a = ParsecT String () (State SourcePos) a
type Name = String
type Value = String
type Content = Maybe String
data Attribute = Attribute { key :: String
                           , value :: String
                           } deriving (Show, Eq)
data Node = Node { tag :: Tag
                 , children :: [Node]
                 } deriving (Show, Eq)
data Tag = Tag { name :: Maybe String
               , attributes :: [Attribute]
               , content :: Content
               } deriving (Show, Eq)

parse :: Parser a -> SourceName -> String -> Either ParseError a
parse aParser source_name input =
  runIndent source_name $ runParserT aParser () source_name input

main :: IO ()
main = do
  let filename = "test.slim"
  text <- readFile filename
  case parse parseDoc filename text of
    Left  err    -> print err
    Right result ->
      compileNodes result 0
      --printNodes result 0

compileNodes :: [Node] -> Int -> IO ()
compileNodes [] _ = return ()
compileNodes (x:xs) indent = do
  compileNode x indent
  compileNodes xs indent

compileNode :: Node -> Int -> IO ()
compileNode node indent = do
  let t = case (name $ tag node) of
            Just n  -> n
            Nothing -> "div"
  let as = attributes $ tag node
  let indentation = replicate indent ' '

  putStrLn $ indentation ++ "<" ++ t ++ " " ++ strip (compileAttrs as) ++ ">"

  case (content $ tag node) of
    Just s  -> putStrLn $ indentation ++ "    " ++ s
    Nothing -> return ()

  compileNodes (children node) (indent + 4)
  putStrLn $ indentation ++ "</" ++ t ++ ">"

tagName :: Tag -> String
tagName tag = do
  case (name tag) of
    Just n  -> n
    Nothing -> "div"

compileAttrs :: [Attribute] -> String
compileAttrs [] = ""
compileAttrs (x:xs) = do
  strip $ compileAttr x ++ " " ++ (compileAttrs xs)

compileAttr :: Attribute -> String
compileAttr a = do
  (key a) ++ "=\"" ++ (value a) ++ "\""

printNode :: Node -> Int -> IO ()
printNode node indent = do
  let nodes = children node
  let indentation = replicate indent ' '
  putStrLn $ indentation ++ (show $ tag node)
  printNodes nodes $ indent + 4

printNodes :: [Node] -> Int -> IO ()
printNodes [] indent = return ()
printNodes (node:nodes) indent = do
  printNode node $ indent
  printNodes nodes $ indent

parseAttrName :: Parser Name
parseAttrName = do
  n <- many1 $ alphaNum <|> char '_' <|> char '-'
  return n

parseAttrValue = do
  oneOf "\'\""
  v <- many1 alphaNum
  oneOf "\'\""
  return v

parseShortAttr :: Parser Attribute
parseShortAttr = do
  s <- oneOf ".#"
  v <- many1 alphaNum
  let n = case s of '.' -> "class"
                    '#' -> "id"
  return $ Attribute n v

ignoreSpaces :: Parser (Maybe String)
ignoreSpaces = do
  optionMaybe $ many1 $ oneOf " \t"

ignoreContentStart :: Parser (Maybe String)
ignoreContentStart = do
  optionMaybe $ many1 $ oneOf " \t|\'"

parseAttr :: Parser Attribute
parseAttr = do
  ignoreSpaces
  a <- parseAttrName
  char '='
  v <- parseAttrValue
  return $ Attribute a v

parseNodes :: [Node] -> Parser [Node]
parseNodes nodes = do
  n <- parseNode
  if emptyNode n
    then return nodes
    else parseNodes $ nodes ++ [n]

parseDoc :: Parser [Node]
parseDoc = do
  ns <- parseNodes []
  return ns

emptyNode :: Node -> Bool
emptyNode node = do
  (children node) == [] && (emptyTag $ tag node)

emptyTag :: Tag -> Bool
emptyTag tag = do
  (attributes tag) == [] && (name tag) == Nothing && (content tag == Nothing)

parseNode :: Parser Node
parseNode = do
  b <- withBlock Node parseTag parseNode
  return b

parseContent :: Parser Content
parseContent = do
  ignoreContentStart
  c <- optionMaybe $ many1 $ noneOf "\n"
  return c

parseTag :: Parser Tag
parseTag = do
  s <- optionMaybe $ many1 alphaNum
  sa <- many $ try parseShortAttr
  la <- many $ try parseAttr
  c <- try parseContent
  spaces
  return $ Tag s (sa ++ la) c
