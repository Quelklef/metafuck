{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Test.HUnit
import Control.Monad (void)

import Value (Value(..))
import Expr (Expr(..), Sigil(..), Name(..))
import Eval (eval)
import Parse (parse)

tests :: Test
tests = test

  [ "parse: plain bf literal"
    ~: Right (LiteralExpr (Brainfuck "++[]<"))
    ~=? parse "++[]<"

  , "parse: fancy bf literal"
    ~: Right (LiteralExpr (Brainfuck ">>>>[+++]<"))
    ~=? parse "4>[3+]<"

  , "parse: simplename binding"
    ~: Right (BindingExpr (Name OffsetSigil "x")
                          (LiteralExpr (Number 3))
                          (LiteralExpr (Brainfuck "+")))
    ~=? parse "&x { +3 } +"

  , "parse: phrase invocation with offset reference arguments"
    ~: Right (InvocationExpr (ReferenceExpr $ Name FunctionSigil "fst * *")
                             [ ReferenceExpr $ Name OffsetSigil "a"
                             , ReferenceExpr $ Name OffsetSigil "b" ])
    ~=? parse "fst &a &b"

  , "parse: phrase invocation with offset literal arguments"
    ~: Right (InvocationExpr (ReferenceExpr $ Name FunctionSigil "fst * *")
                             [ LiteralExpr $ Number 2
                             , LiteralExpr $ Number 3 ])
    ~=? parse "fst +2 +3"

  , "parse: phrase invocation with code"
    ~: Right (InvocationExpr (ReferenceExpr $ Name FunctionSigil "fst *")
                             [ LiteralExpr $ Brainfuck "+" ])
    ~=? parse "fst +"

  , "parse: phrase binding"
    ~: Right (BindingExpr (Name FunctionSigil "fst * *")
                          (FunctionExpr [Name OffsetSigil "a", Name OffsetSigil "b"]
                                        (ReferenceExpr (Name OffsetSigil "a")))
                          (LiteralExpr (Brainfuck "+")))
    ~=? parse "fst &a &b { &a } +"

  , "parse: nested binding"
    ~: Right (BindingExpr (Name FunctionSigil "fst * *")
                          (FunctionExpr [Name OffsetSigil "a", Name OffsetSigil "b"]
                                        (ReferenceExpr (Name OffsetSigil "a")))
                          (BindingExpr (Name FunctionSigil "snd * *")
                                       (FunctionExpr [Name OffsetSigil "x", Name OffsetSigil "y"]
                                                     (ReferenceExpr (Name OffsetSigil "y")))
                                       (LiteralExpr (Brainfuck "+"))))
    ~=? parse "fst &a &b { &a } snd &x &y { &y } +"

  , "parse: simplename after binding"
    ~: Right (BindingExpr (Name FunctionSigil "f *")
                          (FunctionExpr [Name OffsetSigil "x"]
                                        (LiteralExpr (Brainfuck ".")))
                          (ReferenceExpr (Name OffsetSigil "s")))
    ~=? parse "f &x { . } &s"

  , "eval: builtin 'to *'"
    ~: Right (Brainfuck ">>>")
    ~=? eval <$> parse "to +3"

  , "eval: builtin 'from *'"
    ~: Right (Brainfuck "<<<")
    ~=? eval <$> parse "from +3"

  , "eval: builtin while * *'"
    ~: Right (Brainfuck ">>[<<->>]<<")
    ~=? eval <$> parse "while +2 (-)"

  , "eval: custom phrases"
    ~: Right (Brainfuck "-")
    ~=? eval <$> parse [r|
dec { - }
dec
|]

  , "eval: custom phrases #2"
    ~: Right (Brainfuck ">>>[<<<>>>-<<<>>>]<<<")
    ~=? eval <$> parse [r|

dec { - }

on &cell $code {
  to &cell
  $code
  from &cell }

clear &cell {
  while &cell (
    on &cell (dec)) }

&my { +3 }
clear &my

|]

  , "eval: custom phrases #3"
    ~: Right (Brainfuck "[-<+>>+<]<[->+<]>")
    ~=? eval <$> parse [r|

on &cell $do {
  to &cell
  $do
  from &cell
}

while &cell $do {
  to &cell
  [-
    from &cell
    $do
    to &cell
  ]
  from &cell
}

copy &a to &b via &tmp {
  while &a (
    on &tmp (+)
    on &b (+))
  while &tmp (
    on &a (+))
}

copy +0 to +1 via -1

|]

  ]

main :: IO ()
main = void $ runTestTT tests
