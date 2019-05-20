{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where

import           Data.GraphViz
import           Data.GraphViz.Printing        (renderDot)
import           Data.List
import           Data.Strings                  (strEndsWith)
import           Data.Text.Lazy                (toStrict)
import           Data.Text.Lazy.IO             (writeFile)
import           Diagrams.TwoD.GraphViz
import           Lib
import           System.Directory.Tree
import           System.Environment
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token             as Token
import           Text.ParserCombinators.Parsec (noneOf)

data ModuleTree = ModuleTree {
  n            :: String,
  dependencies :: [String]
  }
  deriving (Show)

psDef = haskellStyle
lexer = Token.makeTokenParser psDef
identifier = Token.identifier lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer
comma      = Token.comma      lexer
operator   = Token.operator   lexer
symbol     = Token.symbol     lexer

parseImport = do
  try $ string "import"
  whiteSpace
  importName <- many1 (letter <|> char '.')
  manyTill (noneOf "\n") (char '\n')
  return importName

parseModuleTree = do
  whiteSpace
  string "module"
  whiteSpace
  name <- manyTill (letter <|> oneOf "._'" <|> digit) space
  whiteSpace
  _ <- manyTill (letter <|> oneOf ".,()_=<>:$*-'^/\\|&+#~\n " <|> digit) (try $ string "where")
  whiteSpace
  lst <- sepEndBy parseImport whiteSpace
  pure $ ModuleTree name $ nub lst

rightToMaybe :: Either b a -> Maybe a
rightToMaybe = either (const Nothing) Just

catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

modulesTree :: AnchoredDirTree (Either b ModuleTree) -> [ModuleTree]
modulesTree tree = catMaybes $
        map (\x -> rightToMaybe x) $
        filter (\x -> case x of {Right _ -> True; _ -> False}) $
        map (\x -> file x) $
        (filterPS "purs") $
        flatDirToList $
        flattenDir (dirTree tree)
          where
            pred = \elem -> case elem of
              File _ _ -> True
              _        -> False
            flatDirToList = \t -> filter pred t
            filterPS = \e -> \l -> filter (\elem -> strEndsWith (name elem) e) l

constructDirTree :: FilePath -> IO (AnchoredDirTree (Either ParseError ModuleTree))
constructDirTree path = readDirectoryWith (\h -> parseFromFile parseModuleTree h) path

vertexList :: [ModuleTree] -> [String]
vertexList mt = nub $ [n x | x <- mt] ++ [x | y <- mt, x <- dependencies y]

edgeList :: [ModuleTree] -> [(String, String, ())]
edgeList mt = nub $ mt >>= (\x -> [(n x, y, ()) | y <- dependencies x])

main :: IO ()
main = do
  (project_path:_) <- getArgs
  dirTree <- constructDirTree project_path
  let mT = modulesTree dirTree
  let v = vertexList mT
  let e = edgeList mT
  let gr = mkGraph v e
  let dot = toDot $ graphElemsToDot nonClusteredParams (zip v [1..]) e
  Data.Text.Lazy.IO.writeFile "output.dot" (renderDot dot)



