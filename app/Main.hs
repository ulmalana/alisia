module Main where

import System.Environment


import AlisiaRepl (runRepl, runOne)


main :: IO ()
main = do
   -- putStrLn "Halo!\nAlisia 0.0.1"
   args <- getArgs
   if null args then runRepl else runOne $ args








