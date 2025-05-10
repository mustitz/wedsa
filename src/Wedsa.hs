module Wedsa
  ( CppFile(..)
  , Comment(..)
  , CommentType(..)
  , parseCppFile
  ) where

-- Re-export only the public API
import CppParser (CppFile(..), Comment(..), CommentType(..), parseCppFile)
