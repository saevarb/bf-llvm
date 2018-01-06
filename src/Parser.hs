module Parser where

import Control.Applicative
import Data.Functor
import Data.Either
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data BrainfuckOp
    = IncPtr             -- >
    | DecPtr             -- <
    | IncVal             -- +
    | DecVal             -- -
    | WriteVal           -- .
    | ReadVal            -- ,
    | Loop [BrainfuckOp] -- [ .. ]
    deriving (Show, Read, Eq)

brainfuckP :: Parser [BrainfuckOp]
brainfuckP =
    brainfuckP' <* eof

brainfuckP' :: Parser [BrainfuckOp]
brainfuckP' =
    rights <$> some (eitherP commentCharP (choice parsers))
  where
    parsers =
        [ char '>' $> IncPtr
        , char '<' $> DecPtr
        , char '+' $> IncVal
        , char '-' $> DecVal
        , char '.' $> WriteVal
        , char ',' $> ReadVal
        , Loop <$> between (char '[') (char ']') brainfuckP'
        ]

    commentCharP =
        noneOf ("><+-.,[]" :: String)
