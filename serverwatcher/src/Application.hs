module Application
    ( Application
    , ApplicationPure
    , runApplication
    , embedPure
    ) where

import Control.Monad.Reader
import Control.Monad.Morph
import Control.Monad.Identity

type Application r = ReaderT r (IO)
type ApplicationPure r = ReaderT r (Identity)

runApplication :: Application r a -> r -> IO a
runApplication m r = runReaderT m r

embedPure :: ApplicationPure r a -> Application r a
embedPure x = hoist generalize x


