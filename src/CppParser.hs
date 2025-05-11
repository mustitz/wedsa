module CppParser
  ( CppFile(..)
  , Comment(..)
  , CommentType(..)
  , PP(..)
  , StrLiteral(..)
  , parseCppFile
  , parseComment
  , parseCppComment
  , parsePP
  , parseStr
  , parseContent
  ) where

import Control.Monad.State
import Data.Char (chr)
import Data.Functor.Identity
import Numeric (readHex)
import Text.Parsec

type CppFileParser = Parsec String CppFile

data CppFile = CppFile
  { filePath   :: FilePath     -- Source file path
  , anomalies  :: [Anomaly]    -- Parsing anomalies such as unclosed comments
  , comments   :: [Comment]    -- All comments in the file
  , pps        :: [PP]         -- All preprocessor directives
  , strs       :: [StrLiteral] -- All preprocessor directives
  } deriving (Show)

data Anomaly = Normaly                    -- It is OK
             | UnclosedComment Comment    -- Position of unclosed comment
             | BadEscape SourcePos        -- Position of bad escape sequence
             | UnterminatedStr SourcePos  -- Position of unterminated string
             | MalformedString StrLiteral -- Position of malformed string
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

data StrLiteral = StrLiteral
  { strPos   :: SourcePos  -- Starting position in source
  , strText  :: String     -- Content of a string
  , isStrBad :: Bool       -- Flag indicating str is malformed
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

addStr :: StrLiteral -> CppFileParser ()
addStr s = do
  updateState $ \file -> file { strs = s : strs file }
  when (isStrBad s) $ addAnomaly $ MalformedString s



slashedEol :: ParsecT String u Identity Char
slashedEol = try $ do
  _ <- char '\\'
  endOfLine

cppWhitespace :: CppFileParser ()
cppWhitespace = skipMany $ space <|> slashedEol

withSlashedEol :: CppFileParser a -> CppFileParser a
withSlashedEol p = do
  void $ many slashedEol
  p

cppAnyChar :: CppFileParser Char
cppAnyChar = withSlashedEol anyChar

cppChar :: Char -> CppFileParser Char
cppChar c = withSlashedEol $ char c



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
  _ <- cppChar '#'
  cppWhitespace

  content <- manyTill cppAnyChar (try (void endOfLine) <|> eof)
  let pp = PP pos' content
  addPP pp
  return pp


parseEscapeSequence :: CppFileParser Char
parseEscapeSequence = do
  pos <- getPosition
  c <- cppAnyChar  -- Uses your existing function to handle line continuations
  case c of
    'n' -> return '\n'
    't' -> return '\t'
    'r' -> return '\r'
    '"' -> return '"'
    '\\' -> return '\\'
    '0' -> return '\0'
    'x' -> parseHexEscape pos 2
    'u' -> parseHexEscape pos 4
    _ -> do
      addAnomaly $ BadEscape pos
      fail $ "Invalid escape sequence: \\" ++ [c]

parseHexEscape :: SourcePos -> Int -> CppFileParser Char
parseHexEscape pos n = do
  hexDigits <- count n hexDigit <|> do
    addAnomaly $ BadEscape pos
    fail $ "Expected " ++ show n ++ " hex digits"

  case readHex hexDigits of
    [(val, "")] -> return $ chr val
    _ -> do
      addAnomaly $ BadEscape pos
      fail $ "Invalid hex value: " ++ hexDigits

parseStr :: CppFileParser StrLiteral
parseStr = do
  _ <- cppChar '"'
  pos <- getPosition
  (content, isBad) <- try parseOk <|> parseBad
  let str = StrLiteral pos content isBad
  addStr str
  return str
  where
    parseOk :: CppFileParser (String, Bool)
    parseOk = do
      content <- manyTill goodStrChar $ try $ cppChar '"'
      return (content, False)

    goodStrChar :: CppFileParser Char
    goodStrChar = do
      c <- cppAnyChar
      if c == '\\' then parseEscapeSequence
                   else return c

    parseBad :: CppFileParser (String, Bool)
    parseBad = do
      content <- manyTill badStrChar badStrTerminated
      return (content, True)

    badStrChar :: CppFileParser Char
    badStrChar = do
      c <- anyChar
      if c == '\\' then try parseEscapeSequence <|> badEscapeSequence
                   else return c

    badEscapeSequence :: CppFileParser Char
    badEscapeSequence = do
      pos <- getPosition
      c <- anyChar
      addAnomaly $ BadEscape pos
      return c

    badStrTerminated :: CppFileParser ()
    badStrTerminated = strTerminated <|> nonTerminated

    strTerminated :: CppFileParser ()
    strTerminated = void $ try $ cppChar '"'

    nonTerminated :: CppFileParser ()
    nonTerminated = do
      pos <- getPosition
      try (void endOfLine) <|> eof
      addAnomaly $ UnterminatedStr pos
      return ()

parseTopItem :: CppFileParser ()
parseTopItem = void (try parseComment)
  <|> void (try parseCppComment)
  <|> void (try parsePP)
  <|> void (try parseStr)
  <|> void cppAnyChar

parseContent :: CppFileParser CppFile
parseContent = do
  void $ many parseTopItem
  void eof
  file <- getState
  return $ file
    { comments = reverse (comments file)
    , pps = reverse (pps file)
    , strs = reverse (strs file)
    }

parseCppFile :: FilePath -> String -> Either ParseError CppFile
parseCppFile path content =
  runParser parseContent initialState path content
  where
    initialState = CppFile
      { filePath = path
      , anomalies = []
      , comments = []
      , pps = []
      , strs = []
      }
