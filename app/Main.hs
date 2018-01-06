module Main where

import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Pretty.Simple

import Lib
import Parser

main :: IO ()
main = do
    contents <- getContents
    let result = parse brainfuckP "stdin" contents

    case result of
        Left err ->
            putStrLn $ parseErrorPretty err
        Right ast ->
            pPrint ast
