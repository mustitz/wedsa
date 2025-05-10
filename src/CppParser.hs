module CppParser
  ( CppFile(..)
  , Comment(..)
  , CommentType(..)
  , parseCppFile
  , parseComment
  , parseCppComment
  , parseContent
  ) where

import Control.Monad.State
import Text.Parsec

type CppFileParser = Parsec String CppFile

data CppFile = CppFile
  { filePath   :: FilePath   -- Source file path
  , anomalies  :: [Anomaly]  -- Parsing anomalies such as unclosed comments
  , comments   :: [Comment]  -- All comments in the file
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

addAnomaly :: Anomaly -> CppFileParser ()
addAnomaly anomaly = do
  updateState $ \file -> file { anomalies = anomaly : anomalies file }

addComment :: Comment -> CppFileParser ()
addComment comment = do
  updateState $ \file -> file { comments = comment : comments file }
  when (isCommentBad comment) $ addAnomaly $ UnclosedComment comment

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
          content <- manyTill anyChar (try (string "*/"))
          return (content, False)
        parseUnclosed :: CppFileParser (String, Bool)
        parseUnclosed = do
          content <- manyTill anyChar eof
          return (content, True)

parseCppComment :: CppFileParser Comment
parseCppComment = do
  pos <- getPosition
  _ <- string "//"
  content <- manyTill anyChar (try (void endOfLine) <|> eof)
  let comment = Comment CppStyle pos content False  -- C++ comments can't be "bad" (unclosed)
  addComment comment
  return comment

parseTopItem :: CppFileParser ()
parseTopItem = void (try parseComment)
  <|> void (try parseCppComment)
  <|> void anyChar

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
      }
