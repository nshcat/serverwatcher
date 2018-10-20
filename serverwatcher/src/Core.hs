{-# LANGUAGE ScopedTypeVariables #-}

module Core
    ( run
    ) where

import Control.Monad
import System.Exit
import System.Process
import Control.Concurrent
import System.IO
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Identity

import qualified Application as App    
import Application (runApplication)
import Terminal
import Configuration
import Version

type Application = App.Application Configuration
type ApplicationPure = App.ApplicationPure Configuration


startServer :: Application ProcessHandle
startServer = do
               cfg <- ask
               (_,_,_,ph) <- liftIO $ createProcess $ (shell $ serverPath cfg) { detach_console = True }
               return ph
                   
                   
startServerChecked :: Application (Maybe(ProcessHandle))
startServerChecked = do
                      cfg <- ask
                      ph <- startServer
                      ec <- liftIO $ getProcessExitCode ph
                      case ec of
                        (Just _) -> return Nothing
                        (Nothing) -> return $ Just ph                          
                          
                          
                          
postObserveMsg :: Bool -> IO ()
postObserveMsg b = observeMsgBase postMessage 0 b    

updateObserveMsg :: Int -> Bool -> IO ()
updateObserveMsg x b = observeMsgBase updateMessage x b 

observeMsgBase :: (MessageType -> Bool -> String -> IO()) -> Int -> Bool -> IO()
observeMsgBase f x b = f (InProgress Yellow x) b "Observing process.."

    
observeProcess :: ProcessHandle -> Application ()
observeProcess ph = do
                     cfg <- ask
                     liftIO $ postObserveMsg $ printTime cfg
                     observeProcess' ph 0

observeProcess' :: ProcessHandle -> Int -> Application ()
observeProcess' ph x = do                   
                        ec <- liftIO $ getProcessExitCode ph
                        case ec of
                          (Just _)  -> handleTermination
                          (Nothing) -> do
                                        cfg <- ask
                                        liftIO $ updateObserveMsg x $ printTime cfg
                                        liftIO $ threadDelay 100000
                                        observeProcess' ph (x+1)


handleServerStart :: Application ()
handleServerStart = do
                     ph <- startServerChecked
                     cfg <- ask
                     case ph of
                       (Nothing) -> liftIO $ postMessage Failure (printTime cfg) "Failed to start server!" >> exitFailure
                       (Just ph') -> do
                                      liftIO $ postMessage Success (printTime cfg) "Server started"
                                      observeProcess ph'
                     
                     
handleTermination :: Application ()
handleTermination = let tick x = do
                                  cfg <- ask
                                  let msg = "Waiting " ++ show (10-(x `div` 10)) ++ "s before restarting server.."
                                  liftIO $ updateMessage (InProgress Yellow x) (printTime cfg) msg
                                  liftIO $ threadDelay 100000
                                  :: Application ()
                      in do
                          cfg <- ask
                          liftIO $ updateMessage Failure (printTime  cfg) "Server process terminated!"
                          liftIO $ postMessage (InProgress Yellow 0) (printTime cfg) "Waiting 10s before restarting server.."
                          sequence_ $ map tick [1..100]
                          liftIO $ updateMessage Info (printTime cfg) "Server will now be restarted"
                          handleServerStart


run :: Configuration -> IO ()
run cfg = do
           setTitle $ "ServerWatch: " ++ serverName cfg
           postMessage Info (printTime cfg) $ "This is ServerWatch " ++ appVersion
           postMessage Info (printTime cfg) "Server will now be started"
           runApplication handleServerStart cfg                        
