module Core
    ( run
    ) where

import Control.Monad
import System.Exit
import System.Process
import Control.Concurrent
import System.IO
    
import Terminal
import Configuration
import Version


startServer :: Configuration -> IO(ProcessHandle)
startServer cfg = do
                   (_,_,_,ph) <- createProcess $ (shell $ serverPath cfg) { detach_console = True }
                   return ph
                   
                   
startServerChecked :: Configuration -> IO(Maybe(ProcessHandle))
startServerChecked cfg = do
                          ph <- startServer cfg
                          ec <- getProcessExitCode ph
                          case ec of
                            (Just _) -> return Nothing
                            (Nothing) -> return $ Just ph                          
                          
                          
                          
postObserveMsg :: Bool -> IO ()
postObserveMsg b = observeMsgBase postMessage 0 b    

updateObserveMsg :: Int -> Bool -> IO ()
updateObserveMsg x b = observeMsgBase updateMessage x b 

observeMsgBase :: (MessageType -> Bool -> String -> IO()) -> Int -> Bool -> IO()
observeMsgBase f x b = f (InProgress Yellow x) b "Observing process.."

    
observeProcess :: Configuration -> ProcessHandle -> IO ()
observeProcess cfg ph = do
                         postObserveMsg $ printTime cfg
                         observeProcess' cfg ph 0

observeProcess' :: Configuration -> ProcessHandle -> Int -> IO ()
observeProcess' cfg ph x = do                   
                            ec <- getProcessExitCode ph
                            case ec of
                              (Just _)  -> handleTermination cfg
                              (Nothing) -> do
                                            updateObserveMsg x $ printTime cfg
                                            threadDelay 100000
                                            observeProcess' cfg ph (x+1)


handleServerStart :: Configuration -> IO ()
handleServerStart cfg = do
                         ph <- startServerChecked cfg
                         case ph of
                           (Nothing) -> postMessage Failure (printTime cfg) "Failed to start server!" >> exitFailure
                           (Just ph') -> postMessage Success (printTime cfg) "Server started" >> observeProcess cfg ph'
                     
                     
handleTermination :: Configuration -> IO ()
handleTermination cfg = let tick x = do
                                      let msg = "Waiting " ++ show (10-(x `div` 10)) ++ "s before restarting server.."
                                      updateMessage (InProgress Yellow x) (printTime cfg) msg
                                      threadDelay 100000
                      in do
                          updateMessage Failure (printTime  cfg) "Server process terminated!"
                          postMessage (InProgress Yellow 0) (printTime cfg) "Waiting 10s before restarting server.."
                          sequence_ $ map tick [1..100]
                          updateMessage Info (printTime cfg) "Server will now be restarted"
                          handleServerStart cfg


run :: Configuration -> IO ()
run cfg = do
           setTitle $ "ServerWatch: " ++ serverName cfg
           postMessage Info (printTime cfg) $ "This is ServerWatch " ++ appVersion
           postMessage Info (printTime cfg) "Server will now be started"
           handleServerStart cfg                          
