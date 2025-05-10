module Main where

import Data.Functor.Identity
import Text.Parsec
import Test.Hspec
import Wedsa
import WedsaTesting (parseComment, parseContent, parseCppComment)

initialState :: CppFile
initialState = CppFile
  { filePath = "(test)"
  , anomalies = []
  , comments = []
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


  describe "parseCppFile" $ do
    it "parses an empty file" $ do
      testParseCppFile "" $ \cppFile ->
        length (comments cppFile) `shouldBe` 0

    it "parses a file with a single comment" $ do
      let commentBody = " Test comment "
      testParseCppFile (mkCComment commentBody) $ \cppFile -> do
        length (comments cppFile) `shouldBe` 1
        commentText (head $ comments cppFile) `shouldBe` commentBody
