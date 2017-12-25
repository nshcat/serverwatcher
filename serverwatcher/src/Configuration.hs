module Configuration
    ( Configuration (..)
    , 
    ) where

import System.IO


data Configuration = {
                       serverPath  :: String
                     , serverName  :: String
                     , printTime   :: Bool
                     , handleCtrlC :: Bool
                     }
                     
                
defaultConfig :: Configuration
defaultConfig = Configuration "" "" False False

parseArguments :: IO(Configuration)
parseArguments = undefined
