module Core
    ( run
    ) where

import Control.Monad
import System.Exit
import System.Process
import Control.Concurrent
import System.IO
    
import Terminal


startServer :: String -> IO(ProcessHandle)
startServer path = do {
                      (_,_,_,ph) <- createProcess $ (shell path) { detach_console = True };
                      return ph        
                   }
                   
                   
startServerChecked :: String -> IO(Maybe(ProcessHandle))
startServerChecked path = do {
                            ph <- startServer path;
                            ec <- getProcessExitCode ph;
                            case ec of
                              (Just _) -> return Nothing;
                              (Nothing) -> return $ Just ph                         
                          }  
                          
                          
                          
postObserveMsg :: IO ()
postObserveMsg = observeMsgBase postMessage 0     

updateObserveMsg :: Int -> IO ()
updateObserveMsg x = observeMsgBase updateMessage x

observeMsgBase :: (MessageType -> String -> IO()) -> Int -> IO()
observeMsgBase f x = f (InProgress Yellow x) "Observing process.."

    
observeProcess :: String -> ProcessHandle -> IO ()
observeProcess s ph = do
                       postObserveMsg
                       observeProcess' s ph 0

observeProcess' :: String -> ProcessHandle -> Int -> IO ()
observeProcess' s ph x = do {                   
                           ec <- getProcessExitCode ph;
                           case ec of
                             (Just _)  -> handleTermination s;
                             (Nothing) -> do {
                                            updateObserveMsg x;
                                            threadDelay 100000;
                                            observeProcess' s ph (x+1)
                                          }
                         }

-- TODO: Müssen alle namen des servers mitschleifen.
handleServerStart :: String -> IO ()
handleServerStart s = do {
                        ph <- startServerChecked s;
                        case ph of
                          (Nothing) -> postMessage Failure "Failed to start server!" >> exitFailure;
                          (Just ph') -> postMessage Success "Server started" >> observeProcess s ph'
                      }
                     
                     
handleTermination :: String -> IO ()
handleTermination s = let tick x = do {
                                     let msg = "Waiting " ++ show (10-(x `div` 10)) ++ "s before restarting server.." in
                                     updateMessage (InProgress Yellow x) msg;
                                     threadDelay 100000
                                   }
                      in do {
                           updateMessage Failure "Server process terminated!";
                           postMessage (InProgress Yellow 0) "Waiting 10s before restarting server..";
                           sequence_ $ map tick [1..100];
                           updateMessage Info "Server will now be restarted";
                           handleServerStart s
                          }

run :: String -> IO ()
run s = do {
          postMessage Info "This is ServerWatch v0.0";
          postMessage Info "Server will now be started";
          handleServerStart s
        }                          
