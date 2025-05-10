module CppParser
  ( CppFile(..)
  , Comment(..)
  , CommentType(..)
  , parseCppFile
  , cStyleComment
  ) where

import Data.Maybe (catMaybes)
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)

data CppFile = CppFile {
  filePath   :: FilePath,        -- Source file path
  comments   :: [Comment],       -- All comments in the file
  rawContent :: String           -- Original file content (for reference)
} deriving (Show)

data Comment = Comment {
  commentType :: CommentType,  -- Type of comment
  commentPos  :: SourcePos,    -- Starting position in source
  commentText :: String        -- Content of the comment
} deriving (Show)

data CommentType =
  CStyle     -- /* ... */
  deriving (Show, Eq)

cStyleComment :: Parser Comment
cStyleComment = do
  pos <- getPosition
  _ <- string "/*"
  content <- manyTill anyChar $ try $ string "*/"
  return $ Comment CStyle pos content

commentOrChar :: Parser (Maybe Comment)
commentOrChar = (Just <$> try cStyleComment)
                <|> (void anyChar >> return Nothing)

commentsParser :: Parser [Comment]
commentsParser = do
  results <- many commentOrChar
  return $ catMaybes results

cppFileParser :: FilePath -> String -> Parser CppFile
cppFileParser path content = do
  cmts <- commentsParser
  return $ CppFile {
    filePath = path,
    comments = cmts,
    rawContent = content
  }

parseCppFile :: FilePath -> String -> Either ParseError CppFile
parseCppFile path content = parse (cppFileParser path content) path content
