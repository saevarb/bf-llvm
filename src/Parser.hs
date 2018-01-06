module Parser where

import Text.Megaparsec

type Parser = Parsec String String

data BrainfuckOp
    = IncPtr   -- >
    | DecPtr   -- <
    | IncVal   -- +
    | DecVal   -- -
    | WriteVal -- .
    | ReadVal  -- ,
    | Loop [BrainfuckOp]
    deriving (Show, Read, Eq)

brainfuckP :: Parser String
brainfuckP = undefined
