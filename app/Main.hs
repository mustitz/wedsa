module Main where

import CppParser
import Text.Parsec (sourceLine, sourceColumn)

prettyPrintComment :: Comment -> IO ()
prettyPrintComment (Comment cType pos content) = do
  putStrLn $ "Type: " ++ show cType
  putStrLn $ "Position: Line " ++ show (sourceLine pos) ++
             ", Column " ++ show (sourceColumn pos)
  putStrLn $ "Content: " ++ content
  putStrLn ""

prettyPrintCppFile :: CppFile -> IO ()
prettyPrintCppFile file = do
  putStrLn $ "File: " ++ filePath file
  putStrLn $ "Total comments: " ++ show (length $ comments file)
  putStrLn ""

  putStrLn "Comments:"
  printCommentsWithIndex 1 (comments file)
    where printCommentsWithIndex :: Int -> [Comment] -> IO ()
          printCommentsWithIndex _ [] = return ()
          printCommentsWithIndex i (c:cs) = do
            putStrLn $ "Comment " ++ show i ++ ":"
            prettyPrintComment c
            printCommentsWithIndex (i+1) cs

testCode :: String
testCode = "int main() {\n" ++
          "    /* First comment */\n" ++
          "    int x = 5;\n" ++
          "    /* Second comment */\n" ++
          "    /* Third\n" ++
          "       multi-line\n" ++
          "       comment */\n" ++
          "    return 0;\n" ++
          "}\n"

main :: IO ()
main = do
  putStrLn "Testing with sample C++ code:"
  putStrLn "----------------------------"
  putStrLn testCode
  putStrLn ""

  case parseCppFile "(sample)" testCode of
    Left err -> putStrLn $ "Error: " ++ show err
    Right cppFile -> prettyPrintCppFile cppFile
