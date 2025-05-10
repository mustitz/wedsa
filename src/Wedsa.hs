module Wedsa
  ( CppFile(..)
  , Comment(..)
  , PP(..)
  , StrLiteral(..)
  , CommentType(..)
  , parseCppFile
  ) where

-- Re-export only the public API
import CppParser (CppFile(..), Comment(..), PP(..), StrLiteral(..), CommentType(..), parseCppFile)
