module Main where

import System.Environment
import System.Exit
import Terminal
import Core
import Configuration

main :: IO ()
main = parseArguments >>= run
