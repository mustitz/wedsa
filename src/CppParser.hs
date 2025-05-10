module CppParser
  ( CppFile(..)
  , Comment(..)
  , CommentType(..)
  , PP(..)
  , parseCppFile
  , parseComment
  , parseCppComment
  , parsePP
  , parseContent
  ) where

import Control.Monad.State
import Data.Functor.Identity
import Text.Parsec

type CppFileParser = Parsec String CppFile

data CppFile = CppFile
  { filePath   :: FilePath   -- Source file path
  , anomalies  :: [Anomaly]  -- Parsing anomalies such as unclosed comments
  , comments   :: [Comment]  -- All comments in the file
  , pps        :: [PP]       -- All preprocessor directives
  } deriving (Show)

data Anomaly = Normaly                  -- It is OK
             | UnclosedComment Comment  -- Position of unclosed comment
             deriving (Show)

data Comment = Comment
  { commentType  :: CommentType  -- Type of comment
  , commentPos   :: SourcePos    -- Starting position in source
  , commentText  :: String       -- Content of the comment
  , isCommentBad :: Bool         -- Flag indicating if comment is unclosed
  } deriving (Show)

data CommentType = CStyle     -- /* ... */
                 | CppStyle   -- // ...
                 deriving (Show, Eq)

data PP = PP
  { ppPos  :: SourcePos    -- Starting position in source
  , ppText :: String       -- Content of the preprocessor
  } deriving (Show)

addAnomaly :: Anomaly -> CppFileParser ()
addAnomaly anomaly = do
  updateState $ \file -> file { anomalies = anomaly : anomalies file }

addComment :: Comment -> CppFileParser ()
addComment comment = do
  updateState $ \file -> file { comments = comment : comments file }
  when (isCommentBad comment) $ addAnomaly $ UnclosedComment comment

addPP :: PP -> CppFileParser ()
addPP pp = do
  updateState $ \file -> file { pps = pp : pps file }

slashedEol :: ParsecT String u Identity Char
slashedEol = try $ do
  _ <- char '\\'
  endOfLine

cppWhitespace :: CppFileParser ()
cppWhitespace = skipMany $ space <|> slashedEol

cppAnyChar :: CppFileParser Char
cppAnyChar = do
  void $ many slashedEol
  anyChar

parseComment :: CppFileParser Comment
parseComment = do
  pos <- getPosition
  _ <- string "/*"
  (content, isUnclosed) <- try parseClosed <|> parseUnclosed
  let comment = Comment CStyle pos content isUnclosed
  addComment comment
  return comment
  where parseClosed :: CppFileParser (String, Bool)
        parseClosed = do
          content <- manyTill cppAnyChar (try (string "*/"))
          return (content, False)
        parseUnclosed :: CppFileParser (String, Bool)
        parseUnclosed = do
          content <- manyTill cppAnyChar eof
          return (content, True)

parseCppComment :: CppFileParser Comment
parseCppComment = do
  pos <- getPosition
  _ <- string "//"
  content <- manyTill cppAnyChar (try (void endOfLine) <|> eof)
  let comment = Comment CppStyle pos content False  -- C++ comments can't be "bad" (unclosed)
  addComment comment
  return comment

parsePP :: CppFileParser PP
parsePP = do
  pos <- getPosition
  when (sourceColumn pos /= 1) $
    fail "Preprocessor directives must start at the beginning of a line"

  cppWhitespace
  pos' <- getPosition
  _ <- char '#'
  cppWhitespace

  content <- manyTill cppAnyChar (try (void endOfLine) <|> eof)
  let pp = PP pos' content
  addPP pp
  return pp

parseTopItem :: CppFileParser ()
parseTopItem = void (try parseComment)
  <|> void (try parseCppComment)
  <|> void (try parsePP)
  <|> void cppAnyChar

parseContent :: CppFileParser CppFile
parseContent = do
  void $ many parseTopItem
  void eof
  getState

parseCppFile :: FilePath -> String -> Either ParseError CppFile
parseCppFile path content =
  runParser parseContent initialState path content
  where
    initialState = CppFile
      { filePath = path
      , anomalies = []
      , comments = []
      , pps = []
      }
