module Parse where

import Control.Monad.Identity (Identity)
import Control.Monad (guard)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Either (lefts, rights)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

import Debug.Trace

import Text.Parsec

import Value
import Expr

type Parser a = ParsecT String () Identity a

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

--
println msg = trace (show msg) $ return ()

seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println $ out

uinteger :: Parser Int
uinteger = do
  digits <- many1 digit
  return (read digits :: Int)

integer :: Parser Int
integer = do
  sign <- option '+' (oneOf "+-")
  mag <- uinteger
  return $ if sign == '+'
    then mag
    else negate mag

sigil :: Parser Sigil
sigil = do
  symbol <- anyChar
  case symbol of
    '&' -> return OffsetSigil
    '$' -> return BrainfuckSigil
    _ -> fail "Expected one of &, $"

parenthesized :: Parser a -> Parser a
parenthesized it = char '(' *> it <* char ')'

integerLiteral :: Parser Expr
integerLiteral = LiteralExpr . Number <$> integer

brainfuckLiteral :: Parser Expr
brainfuckLiteral = LiteralExpr . Brainfuck . mconcat <$> many1 brainfuckInstr
  where
    brainfuckInstr = do
      n <- option 1 uinteger
      instr <- oneOf "+-[]<>.,"
      return $ replicate n instr

-- | Parses a name that's just a string of letters
-- | (as opposed to e.g. a phrase name, which can contain spaces)
simpleName :: Parser Name
simpleName = do
  sig <- sigil
  spelling <- many1 letter
  return $ Name sig spelling

simpleReference :: Parser Expr
simpleReference = ReferenceExpr <$> simpleName

-- | Parse a phrase, using the given parser for arguments
phraseOf :: Parser a -> Parser (Name, [a])
phraseOf argParser =
  let phrasePart = try (Left <$> many1 (letter <|> char ' ')) <|> try (Right <$> argParser)
  in do
    _ <- spaces
    parts <- many1 phrasePart
    let nameSpelling = trim . mconcat $ either id (const "*") <$> parts
    let args = rights parts
    -- vv Don't parse e.g. "&a" as a call to function "*" with argument "&a"
    guard $ nameSpelling /= "*"
    return (Name FunctionSigil nameSpelling, args)

phrase :: Parser Expr
phrase = do
  (name, args) <- phraseOf (parenthesized expr <|> try simpleReference <|> try integerLiteral <|> brainfuckLiteral)
  return $ InvocationExpr (ReferenceExpr name) args

-- | Parse a binding, using the given parser for the left-hand side
bindingOf :: Parser a -> Parser (a, Expr, Expr)
bindingOf lhsParser = do
  lhs <- lhsParser
  _ <- spaces >> char '{' >> spaces
  rhs <- expr
  _ <- spaces >> char '}' >> spaces
  body <- expr
  _ <- spaces
  return (lhs, rhs, body)

phraseBinding :: Parser Expr
phraseBinding = do
  ((name, params), defn, body) <- bindingOf (phraseOf simpleName)
  return $ BindingExpr name (FunctionExpr params defn) body

simpleNameBinding :: Parser Expr
simpleNameBinding = do
  (simpleName, defn, body) <- bindingOf simpleName
  return $ BindingExpr simpleName defn body

binding :: Parser Expr
binding = try simpleNameBinding <|> phraseBinding

expr :: Parser Expr
expr = do
  _ <- spaces
  parts <- many1 $ choice
    [ try $ parenthesized expr
    , try brainfuckLiteral
    , try integerLiteral
    , try binding
    , try phrase
    , try $ ReferenceExpr <$> simpleName
    ] <* try spaces

  case parts of
    [x] -> return x
    xs -> return $ SequenceExpr xs

parse :: String -> Either ParseError Expr
parse code = runParser (expr <* eof) () "<anonymous>" code

my :: Parser a -> String -> Either ParseError a
my parser code = runParser parser () "<repl>" code
