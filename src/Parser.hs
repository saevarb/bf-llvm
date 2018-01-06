module Parser where

import Control.Applicative
import Data.Functor

import Text.Megaparsec
import Text.Megaparsec.Char

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

brainfuckP :: Parser [BrainfuckOp]
brainfuckP = do
    some $ choice parsers
  where
    parsers =
        [ char '>' $> IncPtr
        , char '<' $> DecPtr
        , char '+' $> IncVal
        , char '-' $> DecVal
        , char '.' $> WriteVal
        , char ',' $> ReadVal
        , char '[' *> (Loop <$> brainfuckP) <* char ']'
        ]
