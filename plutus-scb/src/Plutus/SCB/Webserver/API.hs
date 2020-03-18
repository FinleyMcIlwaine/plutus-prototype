{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Plutus.SCB.Webserver.API
    ( API
    ) where

import           Plutus.SCB.Webserver.Types (FullReport)
import           Servant.API                ((:<|>), (:>), Capture, Get, JSON, NoContent, Post, ReqBody)

type API
     = "healthcheck" :> Get '[ JSON] ()
       :<|> "all" :> Get '[ JSON] FullReport
