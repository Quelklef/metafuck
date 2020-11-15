module Main where

import Data.Function ((&))
import Data.Functor ((<&>))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))

import Parse (parse)
import Eval (eval)
import Value (Value(..))

main :: IO ()
main = do
  code <- getContents
  case parse code of
    Left err -> do
      hPutStrLn stderr $ "parse error: " <> show err
      exitWith (ExitFailure 1)
    Right parsed -> case eval parsed of
      Brainfuck result -> putStr result
      _ -> error "did not evaluate to brainfuck"

