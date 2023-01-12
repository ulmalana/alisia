module AlisiaRepl where

import Control.Monad.Error
import Control.Monad (liftM)
import System.IO

import AlisiaBasic
import AlisiaParsers
import AlisiaVar
import AlisiaEval

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "muat", String (args !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do
  putStrLn "Halo!\nAlisia 0.2.1" >> primitiveBindings >>= until_ (== "keluar") (readPrompt "Alisia |-> ") . evalAndPrint

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
       then return ()
       else action result >> until_ pred prompt action

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue
