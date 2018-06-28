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
import LLVM.OrcJIT
import LLVM.Target
import LLVM.ExecutionEngine as EE
import Data.Int
import Foreign.Ptr
import qualified Data.ByteString as BS

import Lib
import Parser

helloWorldNoComments :: String
helloWorldNoComments =
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

data Environment =
    Environment
    { bufRef :: Operand
    , idxRef :: Operand
    } deriving (Show)

-- type MyMonad = IRBuilderT (Reader Environment)
type MyMonad = ReaderT Environment IRBuilder
-- newtype MyMonad a = MyMonad (Reader Environment a)
--     deriving (Functor, Applicative, Monad, MonadReader Environment)

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int8) -> (IO Int8)

run :: FunPtr a -> IO Int8
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int8))

runJIT :: AST.Module -> IO ()
runJIT mod = do
    withContext $ \ctx ->
        withModuleFromAST ctx mod $ \m ->
        jit ctx $ \executionEngine ->
        EE.withModuleInEngine executionEngine m $ \ee -> do
        mainfn <- EE.getFunction ee (AST.Name "main")
        case mainfn of
            Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
                return ()
            Nothing -> return ()

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection


emit :: (MonadReader Environment m, MonadFix m, MonadIRBuilder m) => BrainfuckOp -> m ()
emit IncPtr = do
    idx <- asks idxRef
    load idx 0 >>= add (ConstantOperand (Int 8 1)) >>= store idx 0
emit  DecPtr = do
    idx <- asks idxRef
    load idx 0 >>= sub (ConstantOperand (Int 8 1)) >>= store idx 0
emit  IncVal = do
    buf <- asks bufRef
    idxr <- asks idxRef
    ival <- load idxr 0
    pos <- gep buf [ival]
    val <- load pos 0
    add val (ConstantOperand (Int 8 1)) >>= store buf 0
emit  DecVal = do
    buf <- asks bufRef
    idxr <- asks idxRef
    ival <- load idxr 0
    pos <- gep buf [ival]
    val <- load pos 0
    sub val (ConstantOperand (Int 8 1)) >>= store buf 0
emit  (Loop ops) = mdo
    buf <- asks bufRef
    start <- block `named` "start"
    mapM_ (emit ) ops
    val <- load buf 0
    test <- icmp NE val (ConstantOperand (Int 8 0))
    condBr test start exit
    exit <- block `named` "exit"
    return ()
emit  WriteVal = do
    buf <- asks bufRef
    val <- load buf 0
    -- call putch [(val, [])]
    return ()
emit  ReadVal = return ()

main :: IO ()
main = do
    withContext $ \ctx -> do
      withModuleFromAST ctx myModule $ \module' -> do
        moduleLLVMAssembly module' >>= BS.putStrLn
    runJIT myModule
    return ()
  where
    -- parsed = fromRight [] $ parse brainfuckP "stdin" helloWorldNoComments
    parsed = fromRight [] $ parse brainfuckP "stdin" "---++++>><<>>-++-"
    myModule =
        buildModule "testModule" $ do
        memset <- extern (Name "memset") [PointerType i8 (AddrSpace 0), i8, i32] void
        function "main" [] i8 $ \_ -> do
            buf <- alloca i8 (Just $ ConstantOperand (Int 32 32768)) 0 `named` "buffer"
            idx <- alloca i32 (Just $ ConstantOperand (Int 32 1)) 0 `named` "index"
            store idx 0 (ConstantOperand (Int 32 0))
            -- call memset [(buf, []), (ConstantOperand (Int 8 0), []), (ConstantOperand (Int 32 32768), [])]
            let env = Environment buf idx
            runReaderT (mapM_ emit parsed) env
            end <- load buf 0
            ret end
