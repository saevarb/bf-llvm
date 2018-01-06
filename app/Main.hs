{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Either
import Control.Monad.Reader hiding (void)

import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Pretty.Simple

import LLVM.Module
import LLVM.AST as AST hiding (function)
import LLVM.AST.Type
import LLVM.AST.AddrSpace
import LLVM.AST.Constant as C
import LLVM.Internal.Context
import qualified LLVM.AST.Global as G
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.AST.IntegerPredicate

import Lib
import Parser

helloWorldNoComments :: String
helloWorldNoComments =
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

data Environment =
    Environment
    { bufRef :: Constant
    }

-- type MyMonad = IRBuilderT (Reader Environment)
type MyMonad = ReaderT Environment IRBuilder
-- newtype MyMonad a = MyMonad (Reader Environment a)
--     deriving (Functor, Applicative, Monad, MonadReader Environment)


emit :: (MonadFix m, MonadIRBuilder m) => Operand -> BrainfuckOp -> m ()
emit ptr IncPtr =
    gep ptr [ConstantOperand (Int 8 1)] >>= store ptr 0
emit ptr DecPtr =
    gep ptr [ConstantOperand (Int 8 (-1))] >>= store ptr 0
emit ptr IncVal = do
    val <- load ptr 0
    add val (ConstantOperand (Int 8 1)) >>= store val 0
emit ptr DecVal = do
    val <- load ptr 0
    sub val (ConstantOperand (Int 8 1)) >>= store val 0
emit ptr (Loop ops) = mdo
    start <- block `named` "start"
    val <- load ptr 0
    test <- icmp NE val (ConstantOperand (Int 8 0))
    condBr test start exit
    exit <- block `named` "exit"
    return ()
emit ptr WriteVal = return ()
emit ptr ReadVal = return ()

main :: IO ()
main = do
    withContext $ \ctx -> do
      withModuleFromAST ctx myModule $ \module' -> do
        moduleLLVMAssembly module' >>= pPrint
    return ()
  where
    parsed = fromRight [] $ parse brainfuckP "stdin" helloWorldNoComments
    -- parsed = fromRight [] $ parse brainfuckP "stdin" "[+][-]"
    myModule =
        buildModule "testModule" $ do
            -- emitDefn (GlobalDefinition buffer)
            -- emitDefn (GlobalDefinition bufPtr)
            function "main" [] void $ \_ -> do
                buf <- alloca i8 (Just $ ConstantOperand (Int 32 32768)) 0 `named` "buffer"
                mapM (emit buf) parsed
                retVoid
