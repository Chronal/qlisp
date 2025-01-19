{-# LANGUAGE OverloadedStrings #-}

module Lib (
    repl,
  )
where

import Parser (parseSyntax)

import Text.Megaparsec (parseTest)
import qualified Data.Text as T

repl :: IO ()
repl = do
  let fileName = "examples/fib.lisp"
  src <- readFile fileName
  parseTest parseSyntax (T.pack src)
