module Main where

import Text.Parsec
import Test.Hspec
import Wedsa
import WedsaTesting (cStyleComment)

testCStyleCommentOK :: String -> String -> Expectation
testCStyleCommentOK input expectedContent = do
  let result = parse cStyleComment "(test)" input
  case result of
    Left err -> error $ "Should parse C-style comment: " ++ show err
    Right comment -> do
      commentType comment `shouldBe` CStyle
      commentText comment `shouldBe` expectedContent

testCStyleCommentFails :: String -> Expectation
testCStyleCommentFails input = do
  let result = parse cStyleComment "(test)" input
  case result of
    Left _ -> return () -- Expected to fail
    Right _ -> error $ "Parser should fail on malformed input: " ++ input

testParseCppFileOK :: String -> (CppFile -> Expectation) -> Expectation
testParseCppFileOK input validator = do
  let result = parseCppFile "(test)" input
  case result of
    Left err -> error $ "Should parse file: " ++ show err
    Right cppFile -> validator cppFile

mkCComment :: String -> String
mkCComment body = "/*" ++ body ++ "*/"

main :: IO ()
main = hspec $ do

  describe "cStyleComment" $ do
    it "parses an empty comment" $ do
      testCStyleCommentOK "/**/" ""

    it "parses a simple C-style comment" $ do
      let body = " Simple comment "
      testCStyleCommentOK (mkCComment body) body

    it "parses a multi-line C-style comment" $ do
      let body = " First line\n   Second line\n   Third line "
      testCStyleCommentOK (mkCComment body) body

    it "parses a comment with Unicode characters" $ do
      let body = " Über résumé пример 你好 "
      testCStyleCommentOK (mkCComment body) body

    it "fails on unclosed comment" $ do
      testCStyleCommentFails "/* unclosed comment"

    it "fails on malformed comment /*/'" $ do
      testCStyleCommentFails "/*/"

  describe "parseCppFile" $ do
    it "parses an empty file" $ do
      testParseCppFileOK "" $ \cppFile ->
        length (comments cppFile) `shouldBe` 0

    it "parses a file with a single comment" $ do
      let commentBody = " Test comment "
      testParseCppFileOK (mkCComment commentBody) $ \cppFile -> do
        length (comments cppFile) `shouldBe` 1
        commentText (head $ comments cppFile) `shouldBe` commentBody
