module Configuration
    ( Configuration (..)
    , parseArguments
    ) where

import System.IO
import Options.Applicative
import Data.Semigroup ((<>))
import Version

data Configuration = Configuration {
                       serverPath    :: String
                     , serverName    :: String
                     , printTime     :: Bool
                     }
                     

parseArguments :: IO(Configuration)
parseArguments = execParser opts
                   where parser = Configuration <$> argument str (metavar "PATH")
                                                <*> strOption
                                                    ( long "name"
                                                   <> short 'n'
                                                   <> metavar "NAME"
                                                   <> help "Sets the name of the server" )
                                                <*> switch
                                                    ( long "timestamps"
                                                   <> short 'T'
                                                   <> help "Whether to display time stamps" )
                                                                                                                                                   
                         opts   = info (parser <**> helper)
                                   ( fullDesc
                                  <> progDesc "Starts and observes a game server"
                                  <> header ("ServerWatcher " ++ appVersion) )
