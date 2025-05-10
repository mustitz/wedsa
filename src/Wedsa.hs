module Wedsa
  ( CppFile(..)
  , Comment(..)
  , PP(..)
  , CommentType(..)
  , parseCppFile
  ) where

-- Re-export only the public API
import CppParser (CppFile(..), Comment(..), PP(..), CommentType(..), parseCppFile)
