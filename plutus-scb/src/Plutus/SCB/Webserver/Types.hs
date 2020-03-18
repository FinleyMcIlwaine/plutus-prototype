{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Plutus.SCB.Webserver.Types where

import           Data.Aeson        (FromJSON, ToJSON)
import           Data.Map          (Map)
import           Data.UUID         (UUID)
import           GHC.Generics      (Generic)
import           Plutus.SCB.Events (ChainEvent)
import           Plutus.SCB.Types  (ActiveContractState)

data FullReport =
    FullReport
        { latestContractStatus :: Map UUID ActiveContractState
        , events               :: [ChainEvent]
        , eventCount           :: Int
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
