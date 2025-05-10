module Main where

import Text.Parsec
import Test.Hspec
import Wedsa
import WedsaTesting (cStyleComment)

myTest :: String -> Either ParseError CppFile
myTest = parseCppFile "(test)"

main :: IO ()
main = hspec $ do

  describe "cStyleComment" $ do
    it "parses a simple C-style comment" $ do
      let testInput = "/* Simple comment */"
      let result = parse cStyleComment "(test)" testInput
      case result of
        Left err -> error $ "Should parse C-style comment: " ++ show err
        Right comment -> do
          commentType comment `shouldBe` CStyle
          commentText comment `shouldBe` " Simple comment "

  describe "parseCppFile" $ do
    it "parses an empty file" $ do
      let result = myTest ""
      case result of
        Left err -> error $ "Should parse empty file: " ++ show err
        Right cppFile -> length (comments cppFile) `shouldBe` 0
