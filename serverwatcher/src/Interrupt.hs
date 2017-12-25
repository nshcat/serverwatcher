{-# LANGUAGE CPP #-}

module Interrupt
    ( setHandler
    , clearHandler
    ) where
    
import System.Process
import System.IO
import Control.Concurrent

#ifdef mingw32_HOST_OS
import System.Win32.Console.CtrlHandler
#else
import System.Posix.Signals
#endif

import Terminal

#ifdef mingw32_HOST_OS
handlerFunc :: ProcessHandle -> Handle -> ThreadId -> CtrlEvent -> IO (BOOL)
handlerFunc ph out tid cTRL_C_EVENT = let tick x = do
                                                    let msg = "Giving server " ++ show (10-(x `div` 10)) ++ "s to gracefully exit.."
                                                    updateMessage (InProgress Yellow x) msg
                                                    threadDelay 100000
                                      in do
                                          -- Reset handler
                                          clearHandler
                                               
                                          -- Gracefully exit server
                                          hPutStrLn out "save-all"
                                          hPutStrLn out "stop"
                                               
                                          -- Give server time to gracefully exit                                             
                                          sequence_ $ map tick [1..100]
                                         
                                          -- Finally kill process and server (if it didnt already terminate)
                                          killThread tid

handlerFunc _ _ _ _ = return TRUE



#else
handlerFunc :: ProcessHandle -> Handle -> ThreadId -> IO ()
handlerFunc ph out tid = let tick x = do
                                       let msg = "Giving server " ++ show (10-(x `div` 10)) ++ "s to gracefully exit.."
                                       updateMessage (InProgress Yellow x) msg
                                       threadDelay 100000
                         in do
                             -- Reset handler
                             clearHandler
                                   
                             -- Gracefully exit server
                             hPutStrLn out "save-all"
                             hPutStrLn out "stop"
                                   
                             -- Give server time to gracefully exit                                             
                             sequence_ $ map tick [1..100]
                             
                             -- Finally kill process and server (if it didnt already terminate)
                             killThread tid
                             
#endif                          


setHandler :: ProcessHandle -> Handle -> IO ()
#ifdef mingw32_HOST_OS
setHandler ph out = do
                     tid <- myThreadId
                     hndlr <- mkHandler $ handlerFunc ph out tid
                     c_SetConsoleCtrlHandler hndlr True
                     return ()
#else
setHandler ph out = do
                     tid <- myThreadId
                     installHandler keyboardSignal (Catch $ handlerFunc ph out tid) Nothing
                     return ()
#endif




clearHandler :: IO ()
#ifdef mingw32_HOST_OS
clearHandler = return () -- TODO
#else
clearHandler = installHandler keyboardSignal Default Nothing >> return ()
#endif


