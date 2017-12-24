module Main where

import System.Environment
import System.Exit
import Terminal
import Core

main :: IO ()
main = do {
         args <- getArgs;
         case args of
           [s] -> run s;
           _   -> do {
                    postMessage Failure "Expected exactly one argument!";
                    postMessage Info "Usage: ServerWatch.hs <path to server>";
                    exitFailure
                  }
       }
