module Terminal
    ( MessageType (..)
    , module System.Console.ANSI -- TODO: find way to only export color constructors
    , postMessage
    , updateMessage
    ) where

import System.Console.ANSI
import Data.Time
import Control.Monad


data MessageType = Info | Success | Failure | InProgress Color Int


typeSGR :: MessageType -> [SGR]
typeSGR Success = [SetColor Foreground Vivid Green]
typeSGR Failure = [SetColor Foreground Vivid Red]
typeSGR Info    = [SetColor Foreground Vivid White]
typeSGR _       = error "Can't use typeSGR with InProgress"                             


typeText :: MessageType -> String
typeText Success = "  OK  "
typeText Failure = " FAIL "
typeText Info    = " INFO "
typeText _       = error "Invalid MessageType!"

inProgressText :: Int -> String
inProgressText x = states !! x'
                     where x' = mod x $ length states;
                           states   = (++) states' $ reverse {-. init-} $ states';
                           states'  = map genStr [0..7];
                           genStr x = concat [prefix, str, postfix]
                                       where startPos = max 0 $ x - 2;
                                             endPos   = min 5 x;
                                             len      = endPos - (startPos - 1);
                                             str      = replicate len '*';
                                             prefix   = replicate startPos ' ';
                                             postfix  = replicate (5 - endPos) ' '


selectOrder :: (Monad m) => m () -> m () -> Bool -> m ()
selectOrder mA mB b = if b
                        then mA >> mB
                        else mB >> mA


timeStampFormat :: String
timeStampFormat = "[%H:%M:%S] "


renderTimestamp :: IO (String)
renderTimestamp =  liftM (formatTime defaultTimeLocale timeStampFormat) $ getZonedTime



renderMsgType :: MessageType -> IO ()
renderMsgType (InProgress s i) = renderInProgress s i
renderMsgType t                = do
                                  putChar '['
                                  setSGR $ typeSGR t
                                  putStr $ typeText t
                                  setSGR []
                                  putChar ']'
                                  putChar ' '
                                 
                                
                
renderInProgress :: Color -> Int -> IO ()
renderInProgress s i = do
                        putChar '['
                        setSGR [SetColor Foreground Vivid s]
                        putStr $ inProgressText i
                        setSGR []
                        putChar ']'
                        putChar ' '

                       
postMessage :: MessageType -> Bool -> String -> IO ()
postMessage t ts msg = do
                        when ts $ renderTimestamp >>= putStr
                        renderMsgType t
                        putStrLn msg

                
updateMessage :: MessageType -> Bool -> String -> IO ()
updateMessage t ts msg = do
                          cursorUpLine 1
                          clearLine
                          postMessage t ts msg

