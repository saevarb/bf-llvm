module Main where

import Text.Megaparsec
import Text.Pretty.Simple

import Lib
import Parser

testProgram :: String
testProgram =
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

main :: IO ()
main = do
    pPrint $ parse brainfuckP "test" testProgram
