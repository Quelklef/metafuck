module Eval where

import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad.Reader (Reader, runReader, ask, local)

import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map

import Value (Value(..))
import Expr (Expr(..), Name(..), Sigil(..), getSpelling)
import qualified Value

updateMap :: (Ord k) => Map.Map k v -> [(k, v)] -> Map.Map k v
updateMap theMap = foldr (\(k, v) m -> Map.insert k v m) theMap

--

-- evaluates an AST node
-- undefined on poorly-typed ASTs
eval :: Expr -> Value
eval node = runReader (evalAux node) builtins

type Scope = Map.Map Name Value

builtins :: Scope
builtins = Map.fromList
  [ (Name FunctionSigil "to *", Function $ \args -> case args of
      [Number offset] -> Brainfuck $ offsetToBrainfuck offset
      _ -> error "bad arguments to function 'to *'")

  , (Name FunctionSigil "from *", Function $ \args -> case args of
      [Number offset] -> Brainfuck $ offsetToBrainfuck (negate offset)
      _ -> error "bad arguments to function 'from *'")

  , (Name FunctionSigil "while * *", Function $ \args -> case args of
      [Number offset, Brainfuck body] -> Brainfuck $ mconcat
        [ offsetToBrainfuck offset
        , "["
        , offsetToBrainfuck (negate offset)
        , body
        , offsetToBrainfuck offset
        , "]"
        , offsetToBrainfuck (negate offset)
        ]
      _ -> error "bad arguments to function 'while * *'")
  ]
  where
    offsetToBrainfuck offset
      | offset < 0 = replicate (negate offset) '<'
      | otherwise = replicate offset '>'


evalAux :: Expr -> Reader Scope Value

evalAux (LiteralExpr val) = return val

evalAux (ReferenceExpr name) = do
  scope <- ask
  case Map.lookup name scope of
    Nothing -> error $ "attempt to reference nonexistent variable " <> show (getSpelling name)
    Just val -> return val

evalAux (FunctionExpr paramNames body) = do
  scope <- ask
  return . Function $ \paramVals -> do
    let definitions = zip paramNames paramVals
    let newScope = updateMap scope definitions
    runReader (evalAux body) newScope

evalAux (InvocationExpr funcExpr argExprs) = do
  func <- evalAux funcExpr
  args <- sequence $ evalAux <$> argExprs
  return $ (Maybe.fromJust $ Value.asFunction func) args

evalAux (SequenceExpr nodes) = do
  eval'd <- sequence (evalAux <$> nodes)
  let catted = eval'd <&> (Maybe.fromJust . Value.asBrainfuck) & mconcat
  return $ Brainfuck catted

evalAux (BindingExpr name defn body) = do
  value <- evalAux defn
  local (Map.insert name value) (evalAux body)
