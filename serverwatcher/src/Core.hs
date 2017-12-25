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
                          
                          
                          
postObserveMsg :: IO ()
postObserveMsg = observeMsgBase postMessage 0     

updateObserveMsg :: Int -> IO ()
updateObserveMsg x = observeMsgBase updateMessage x

observeMsgBase :: (MessageType -> String -> IO()) -> Int -> IO()
observeMsgBase f x = f (InProgress Yellow x) "Observing process.."

    
observeProcess :: Configuration -> ProcessHandle -> IO ()
observeProcess cfg ph = do
                         postObserveMsg
                         observeProcess' cfg ph 0

observeProcess' :: Configuration -> ProcessHandle -> Int -> IO ()
observeProcess' cfg ph x = do                   
                            ec <- getProcessExitCode ph
                            case ec of
                              (Just _)  -> handleTermination cfg
                              (Nothing) -> do
                                            updateObserveMsg x
                                            threadDelay 100000
                                            observeProcess' cfg ph (x+1)


handleServerStart :: Configuration -> IO ()
handleServerStart cfg = do
                         ph <- startServerChecked cfg
                         case ph of
                           (Nothing) -> postMessage Failure "Failed to start server!" >> exitFailure
                           (Just ph') -> postMessage Success "Server started" >> observeProcess cfg ph'
                     
                     
handleTermination :: Configuration -> IO ()
handleTermination cfg = let tick x = do
                                      let msg = "Waiting " ++ show (10-(x `div` 10)) ++ "s before restarting server.."
                                      updateMessage (InProgress Yellow x) msg
                                      threadDelay 100000
                      in do
                          updateMessage Failure "Server process terminated!"
                          postMessage (InProgress Yellow 0) "Waiting 10s before restarting server.."
                          sequence_ $ map tick [1..100]
                          updateMessage Info "Server will now be restarted"
                          handleServerStart cfg


run :: Configuration -> IO ()
run cfg = do 
           postMessage Info $ "This is ServerWatch " ++ appVersion
           postMessage Info "Server will now be started"
           handleServerStart cfg                          
