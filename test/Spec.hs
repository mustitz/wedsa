module Main where

import Data.Functor.Identity
import Text.Parsec
import Test.Hspec
import Wedsa
import WedsaTesting (parseComment, parseContent, parseCppComment, parsePP, parseStr)

initialState :: CppFile
initialState = CppFile
  { filePath = "(test)"
  , anomalies = []
  , comments = []
  , pps = []
  , strs = []
  }

testRun :: Stream s Identity t => Parsec s CppFile a -> s -> a
testRun f input =
  let result = runParser f initialState "(test)" input
  in case result of
    Left err -> error $ "Parser should always parse! Error: " ++ show err
    Right value -> value

testCStyleComment :: String -> String -> Bool -> Expectation
testCStyleComment input expectedContent isBad = do
  let comment = testRun parseComment input
  commentType comment `shouldBe` CStyle
  commentText comment `shouldBe` expectedContent
  isCommentBad comment `shouldBe` isBad

testCppStyleComment :: String -> String -> Expectation
testCppStyleComment input expectedContent = do
  let comment = testRun parseCppComment input
  commentType comment `shouldBe` CppStyle
  commentText comment `shouldBe` expectedContent
  isCommentBad comment `shouldBe` False

testPP :: String -> String -> Expectation
testPP input expectedContent = do
  let pp = testRun parsePP input
  ppText pp `shouldBe` expectedContent

testStr :: String -> String -> Bool -> Expectation
testStr input expectedContent isBad = do
  let str = testRun parseStr input
  strText str `shouldBe` expectedContent
  isStrBad str `shouldBe` isBad

testParseCppFile :: String -> (CppFile -> Expectation) -> Expectation
testParseCppFile input validator =
  let file = testRun parseContent input
  in validator file

mkCComment :: String -> String
mkCComment body = "/*" ++ body ++ "*/"

main :: IO ()
main = hspec $ do

  describe "parseComment" $ do
    it "parses an empty comment" $ do
      testCStyleComment "/**/" "" False

    it "parses a simple C-style comment" $ do
      let body = " Simple comment "
      testCStyleComment (mkCComment body) body False

    it "parses a multi-line C-style comment" $ do
      let body = " First line\n   Second line\n   Third line "
      testCStyleComment (mkCComment body) body False

    it "parses a comment with Unicode characters" $ do
      let body = " Über résumé пример 你好 "
      testCStyleComment (mkCComment body) body False

    it "fails on unclosed comment" $ do
      let body = " unclosed comment"
      testCStyleComment ("/*" ++ body) body True

    it "fails on malformed comment /*/'" $ do
      testCStyleComment "/*/" "/" True


  describe "parseCppComment" $ do
    it "parses a simple C++ style comment" $ do
      testCppStyleComment "// Simple comment" " Simple comment"

    it "parses a C++ style comment with spaces" $ do
      testCppStyleComment "//    Indented comment" "    Indented comment"

    it "parses a C++ style comment until end of line" $ do
      testCppStyleComment "// Comment\nNext line" " Comment"

    it "parses a C++ style comment with special characters" $ do
      testCppStyleComment "///// Symbols: !@#$%^&*()" "/// Symbols: !@#$%^&*()"

    it "parses a C++ style comment with Unicode characters" $ do
      testCppStyleComment "// Über résumé пример 你好" " Über résumé пример 你好"

    it "handles empty C++ style comments" $ do
      testCppStyleComment "//" ""


  describe "parsePP" $ do
    it "parses a basic preprocessor directive" $ do
      testPP "#include <stdio.h>\n" "include <stdio.h>"

    it "parses a preprocessor directive with whitespace" $ do
      testPP "  #define MAX 100" "define MAX 100"

    it "parses an empty preprocessor directive" $ do
      testPP "#" ""

    it "parses a directive with symbols and numbers" $ do
      testPP "#if (VERSION >= 2)" "if (VERSION >= 2)"

    it "handles multi-word directives" $ do
      testPP "#pragma once" "pragma once"

    it "captures directive until end of line" $ do
      testPP "#define PI 3.14159\nint main() {" "define PI 3.14159"

    it "multiline preprocessor directive" $ do
      testPP "#define QW\\\nERTY 0" "define QWERTY 0"

    it "strange preprocessor directive" $ do
      testPP "#de\\\n\\\nfi\\\nne QW\\\nERTY 0" "define QWERTY 0"


  describe "parseStr" $ do
    it "parses an empty string literal" $ do
      testStr "\"\"" "" False

    it "parses a simple string literal" $ do
      testStr "\"Hello, world!\"" "Hello, world!" False

    it "parses a string literal with escape sequences" $ do
      testStr "\"Hello\\nWorld\\t!\"" "Hello\nWorld\t!" False

    it "parses a string literal with escaped quotes" $ do
      testStr "\"He said \\\"Hello\\\"\"" "He said \"Hello\"" False

    it "parses a string literal with hex escape" $ do
      testStr "\"\\x41\\x42\\x43\"" "ABC" False

    it "parses a string literal with Unicode escape sequences" $ do
      testStr "\"Unicode: \\u0041\\u0042\\u0043\"" "Unicode: ABC" False

    it "parses a multiline string literal" $ do
      testStr "\"\\\n123\\\n456\"" "123456" False

    it "handles unterminated string literals" $ do
      testStr "\"Unterminated\n" "Unterminated" True

    it "handles unterminated string literals with escaped quot" $ do
      testStr "\"Unterminated\\\"\n" "Unterminated\"" True

    it "handles bad escape sequence" $ do
      testStr "\"\\q\"" "q" True

    it "handles bad escape sequence, unterminated" $ do
      testStr "\"\\q" "q" True

    it "handles bad \\x sequence" $ do
      testStr "\"\\x1xx" "x1xx" True

    it "handles bad \\u sequence" $ do
      testStr "\"\\u111u" "u111u" True

  describe "parseCppFile" $ do
    it "parses an empty file" $ do
      testParseCppFile "" $ \cppFile ->
        length (comments cppFile) `shouldBe` 0

    it "parses a file with a single comment" $ do
      let commentBody = " Test comment "
      testParseCppFile (mkCComment commentBody) $ \cppFile -> do
        length (comments cppFile) `shouldBe` 1
        commentText (head $ comments cppFile) `shouldBe` commentBody

    it "parses a file with a preprocessor directive" $ do
      testParseCppFile "#include <stdio.h>" $ \cppFile -> do
        length (pps cppFile) `shouldBe` 1
        ppText (head $ pps cppFile) `shouldBe` "include <stdio.h>"

    it "parses a file with mixed comments and preprocessor directives" $ do
      let content = "// Header comment\n#include <stdio.h>\n/* Body comment */\n#define MAX 100"
      testParseCppFile content $ \cppFile -> do
        length (comments cppFile) `shouldBe` 2
        length (pps cppFile) `shouldBe` 2
        ppText (head $ pps cppFile) `shouldBe` "define MAX 100"
        ppText (pps cppFile !! 1) `shouldBe` "include <stdio.h>"

    it "rejects preprocessor directives not at start of line" $ do
      testParseCppFile "int main() { #define MAX 100 }" $ \cppFile -> do
        length (pps cppFile) `shouldBe` 0
