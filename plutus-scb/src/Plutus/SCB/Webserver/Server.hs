{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Plutus.SCB.Webserver.Server
    ( main
    ) where

import           Control.Monad.Except       (ExceptT (ExceptT))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger       (MonadLogger, logInfoN)
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Monoid                (getSum)
import           Data.Proxy                 (Proxy (Proxy))
import           Eventful                   (streamEventEvent)
import           Network.Wai.Handler.Warp   (run)
import           Plutus.SCB.App             (App, runApp)
import           Plutus.SCB.Arbitrary       ()
import           Plutus.SCB.Core            (MonadEventStore, runGlobalQuery)
import           Plutus.SCB.Events          (ChainEvent)
import qualified Plutus.SCB.Query           as Query
import           Plutus.SCB.Types           (Config, WebserverConfig (..), scbWebserverConfig)
import           Plutus.SCB.Utils           (tshow)
import           Plutus.SCB.Webserver.API   (API)
import           Plutus.SCB.Webserver.Types
import           Servant                    ((:<|>) ((:<|>)), Application, Handler (Handler), err500, errBody,
                                             hoistServer, serve)
import           Servant.Client             (BaseUrl (baseUrlPort))

asHandler :: Config -> App a -> Handler a
asHandler config action =
    Handler $
    ExceptT $
    fmap (first (\err -> err500 {errBody = LBS.pack $ show err})) $
    runApp config action

healthcheck :: Monad m => m ()
healthcheck = pure ()

fullReport :: App FullReport
fullReport = do
    latestContractStatus <- runGlobalQuery Query.latestContractStatus
    events <- fmap streamEventEvent <$> runGlobalQuery Query.pureProjection
    eventCount <- getSum <$> runGlobalQuery Query.eventCount
    pure FullReport {latestContractStatus, eventCount, events}

app :: Config -> Application
app config =
    serve (Proxy @API) $
    hoistServer (Proxy @API) (asHandler config) $ healthcheck :<|> fullReport

main ::
       (MonadIO m, MonadLogger m, MonadEventStore ChainEvent m)
    => Config
    -> m ()
main config = do
    let port = baseUrlPort $ baseUrl $ scbWebserverConfig config
    logInfoN $ "Starting SCB backend server on port: " <> tshow port
    liftIO $ run port $ app config
