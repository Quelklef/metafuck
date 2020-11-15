module Expr where

import Data.Function (on, (&))
import Data.Char (toLower, isSpace)

import Value (Value)

data Sigil
  = OffsetSigil
  | BrainfuckSigil
  | FunctionSigil
  deriving (Show, Ord, Eq)

data Name = Name Sigil String
  deriving (Show, Ord)

getSpelling :: Name -> String
getSpelling (Name _ spelling) = spelling

normalizeSpelling :: String -> String
normalizeSpelling spelling =
    spelling
    & fmap toLower
    & collapseWhitespace
  where
    collapseWhitespace :: String -> String
    collapseWhitespace (ca:cb:cs)
      | isSpace ca && isSpace cb       = collapseWhitespace (cb:cs)
      | isSpace ca && not (isSpace cb) = ' ' : collapseWhitespace (cb:cs)
      | otherwise                      = ca : collapseWhitespace (cb:cs)
    collapseWhitespace s = s

instance Eq Name where
  Name sig spelling == Name sig' spelling' =
    (sig, normalizeSpelling spelling) == (sig', normalizeSpelling spelling')

data Expr
  = LiteralExpr Value
  | ReferenceExpr Name
  | BindingExpr Name Expr Expr
  | SequenceExpr [Expr]
  | FunctionExpr [Name] Expr
  | InvocationExpr Expr [Expr]
  deriving (Show, Eq)
