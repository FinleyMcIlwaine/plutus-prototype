{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.SCB.Core
    ( dbConnect
    , installContract
    , activateContract
    , reportContractStatus
    , installedContracts
    , installedContractsProjection
    , activeContracts
    , txHistory
    , txHistoryProjection
    , activeContractsProjection
    , activeContractHistory
    , Connection(Connection)
    , ContractCommand(..)
    , invokeContract
    , refreshProjection
    , runCommand
    , runGlobalQuery
    , updateContract
    , addProcessBus
    , Source(..)
    , toUUID
    -- * Effects
    , ContractEffects
    ) where

import           Control.Monad                              (void)
import           Control.Monad.Freer                        (Eff, Member, Members)
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Extra.Log
import           Control.Monad.IO.Unlift                    (MonadUnliftIO)
import           Control.Monad.Logger                       (MonadLogger)
import qualified Control.Monad.Logger                       as MonadLogger
import           Data.Aeson                                 (withObject, (.:))
import qualified Data.Aeson                                 as JSON
import           Data.Aeson.Types                           (Parser)
import qualified Data.Aeson.Types                           as JSON
import           Data.Foldable                              (traverse_)
import           Data.Map.Strict                            (Map)
import qualified Data.Map.Strict                            as Map
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Data.Text.Prettyprint.Doc                  (pretty, (<+>))
import           Database.Persist.Sqlite                    (createSqlitePoolFromInfo, mkSqliteConnectionInfo)
import           Eventful                                   (Projection, StreamEvent (..), UUID, projectionMapMaybe)
import           Eventful.Store.Sql                         (defaultSqlEventStoreConfig)
import           Language.Plutus.Contract.Effects.OwnPubKey (OwnPubKeyRequest)
import qualified Language.Plutus.Contract.Wallet            as Wallet
import qualified Ledger
import qualified Ledger.Constraints                         as Ledger
import           Plutus.SCB.Command                         (installCommand, saveBalancedTx, saveBalancedTxResult,
                                                             saveContractState)
import           Plutus.SCB.Effects.Contract
import           Plutus.SCB.Effects.EventLog
import           Plutus.SCB.Effects.UUID

import           Plutus.SCB.Events                          (ChainEvent (NodeEvent, UserEvent), NodeEvent (SubmittedTx),
                                                             UserEvent (ContractStateTransition, InstallContract))
import           Plutus.SCB.Query                           (latestContractStatus, monoidProjection, setProjection)
import           Plutus.SCB.Types                           (ActiveContract (ActiveContract),
                                                             ActiveContractState (ActiveContractState),
                                                             Contract (Contract), DbConfig (DbConfig),
                                                             PartiallyDecodedResponse,
                                                             SCBError (ActiveContractStateNotFound, ContractCommandError, ContractNotFound),
                                                             activeContract, activeContractId, activeContractPath,
                                                             contractPath, dbConfigFile, dbConfigPoolSize, hooks,
                                                             newState, partiallyDecodedResponse)
import           Plutus.SCB.Utils                           (render, tshow)
import qualified Wallet.API                                 as WAPI
import           Wallet.Emulator.MultiAgent                 (EmulatedWalletEffects)

type ContractEffects =
        '[ EventLogEffect ChainEvent
         , Log
         , UUIDEffect
         , ContractEffect
         , Error SCBError
         ]

installContract ::
    ( Member Log effs
    , Member (EventLogEffect ChainEvent) effs
    )
    => FilePath
    -> Eff effs ()
installContract filePath = do
    logInfo $ "Installing: " <> tshow filePath
    void $
        runCommand
            installCommand
            UserEventSource
            (Contract {contractPath = filePath})
    logInfo "Installed."

installedContracts :: Member (EventLogEffect ChainEvent) effs => Eff effs (Set Contract)
installedContracts = runGlobalQuery installedContractsProjection

lookupContract ::
    ( Member (EventLogEffect ChainEvent) effs
    , Member (Error SCBError) effs
    )
    => FilePath
    -> Eff effs Contract
lookupContract filePath = do
    installed <- installedContracts
    let matchingContracts =
            Set.filter
                (\Contract {contractPath} -> contractPath == filePath)
                installed
    case Set.lookupMin matchingContracts of
        Just c  -> pure c
        Nothing -> throwError (ContractNotFound filePath)

activateContract ::
    --    ( MonadIO m
    --    , MonadLogger m
    --    , MonadEventStore ChainEvent m
    --    , MonadContract m
    --    , MonadError SCBError m
    --    )
    ( Member Log effs
    , Member (EventLogEffect ChainEvent) effs
    , Member (Error SCBError) effs
    , Member UUIDEffect effs
    , Member ContractEffect effs
    )
    => FilePath
    -> Eff effs UUID
activateContract filePath = do
    logInfo "Finding Contract"
    contract <- lookupContract filePath
    activeContractId <- uuidNextRandom
    logInfo "Initializing Contract"
    response <- invokeContract $ InitContract (contractPath contract)
    let activeContractState =
            ActiveContractState
                { activeContract =
                      ActiveContract
                          { activeContractId
                          , activeContractPath = contractPath contract
                          }
                , partiallyDecodedResponse = response
                }
    logInfo "Storing Initial Contract State"
    void $ runCommand saveContractState ContractEventSource activeContractState
    logInfo . render $
        "Activated:" <+> pretty (activeContract activeContractState)
    logInfo "Done"
    pure activeContractId

updateContract ::
       ( Members EmulatedWalletEffects effs
       , Member Log effs
       , Member (Error SCBError) effs
       , Member (EventLogEffect ChainEvent) effs
       , Member ContractEffect effs
       )
    --        MonadLogger m
    --    , MonadEventStore ChainEvent m
    --    , MonadContract m
    --    , MonadError SCBError m
    --    , WalletAPI m
    --    , NodeAPI m
    --    , WalletDiagnostics m
    --    , ChainIndexAPI m
    --    )
    => UUID
    -> Text
    -> JSON.Value
    -> Eff effs ()
updateContract uuid endpointName endpointPayload = do
    logInfo "Finding Contract"
    oldContractState <- lookupActiveContractState uuid
    logInfo "Updating Contract"
    response <-
        invokeContractUpdate oldContractState endpointName endpointPayload
    let newContractState =
            oldContractState {partiallyDecodedResponse = response}
    logInfo . render $ "Updated:" <+> pretty newContractState
    logInfo "Storing Updated Contract State"
    void $ runCommand saveContractState ContractEventSource newContractState
    --
    logInfo "Handling Resulting Blockchain Events"
    handleBlockchainEvents response
    logInfo "Done"

handleBlockchainEvents ::
       ( Members EmulatedWalletEffects effs
       , Member Log effs
       , Member (Error SCBError) effs
       , Member (EventLogEffect ChainEvent) effs
       )
    => PartiallyDecodedResponse
    -> Eff effs ()
handleBlockchainEvents response = do
    handleTxHook response
    handleUtxoAtHook response
    handleOwnPubKeyHook response

parseSingleHook ::
    ( Member (Error SCBError) effs
    )
    => (JSON.Value -> Parser a)
    -> PartiallyDecodedResponse
    -> Eff effs a
parseSingleHook parser response =
    case JSON.parseEither parser (hooks response) of
        Left err     -> throwError $ ContractCommandError 0 $ Text.pack err
        Right result -> pure result

handleTxHook ::
       ( Members EmulatedWalletEffects effs
       , Member Log effs
       , Member (Error SCBError) effs
       , Member (EventLogEffect ChainEvent) effs
       )
    => PartiallyDecodedResponse
    -> Eff effs ()
handleTxHook response = do
    logInfo "Handling 'tx' hook."
    unbalancedTxs <- parseSingleHook txKeyParser response
    logInfo $ "Balancing unbalanced TXs: " <> tshow unbalancedTxs
    balancedTxs <- traverse Wallet.balanceWallet unbalancedTxs
    traverse_ (runCommand saveBalancedTx WalletEventSource) balancedTxs
    --
    logInfo $ "Submitting balanced TXs: " <> tshow unbalancedTxs
    balanceResults <- traverse submitTxn balancedTxs
    traverse_ (runCommand saveBalancedTxResult NodeEventSource) balanceResults

handleUtxoAtHook ::
       ( Member Log effs
       , Member (Error SCBError) effs
       )
    => PartiallyDecodedResponse
    -> Eff effs ()
handleUtxoAtHook response = do
    logInfo "Handling 'utxo-at' hook."
    utxoAts <- parseSingleHook utxoAtKeyParser response
    traverse_ (logInfo . tshow) utxoAts
    logWarn "UNIMPLEMENTED: handleUtxoAtHook"

handleOwnPubKeyHook ::
       ( Member (Error SCBError) effs
       , Member Log effs
       )
    => PartiallyDecodedResponse
    -> Eff effs ()
handleOwnPubKeyHook response = do
    logInfo "Handling 'own-pubkey' hook."
    ownPubKeys <- parseSingleHook ownPubKeyParser response
    logInfo $ tshow ownPubKeys
    logWarn "UNIMPLEMENTED: handleOwnPubKeyHook"

-- | A wrapper around the NodeAPI function that returns some more
-- useful evidence of the work done.
submitTxn :: (Members EmulatedWalletEffects effs) => Ledger.Tx -> Eff effs Ledger.Tx
submitTxn txn = do
    WAPI.submitTxn txn
    pure txn

txKeyParser :: JSON.Value -> Parser [Ledger.UnbalancedTx]
txKeyParser = withObject "tx key" $ \o -> o .: "tx"

utxoAtKeyParser :: JSON.Value -> Parser [Ledger.Address]
utxoAtKeyParser = withObject "utxo-at key" $ \o -> o .: "utxo-at"

ownPubKeyParser :: JSON.Value -> Parser OwnPubKeyRequest
ownPubKeyParser = withObject "own-pubkey key" $ \o -> o .: "own-pubkey"

reportContractStatus ::
    --    (MonadLogger m, MonadEventStore ChainEvent m) => UUID -> m ()
    ( Member Log effs
    , Member (EventLogEffect ChainEvent) effs
    )
    => UUID
    -> Eff effs ()
reportContractStatus uuid = do
    logInfo "Finding Contract"
    statuses <- runGlobalQuery latestContractStatus
    logInfo $ render $ pretty $ Map.lookup uuid statuses

lookupActiveContractState ::
    --    MonadEventStore ChainEvent m
    ( Member (Error SCBError) effs
    , Member (EventLogEffect ChainEvent) effs
    )
    => UUID
    -> Eff effs ActiveContractState
lookupActiveContractState uuid = do
    active <- activeContractStates
    case Map.lookup uuid active of
        Nothing -> throwError (ActiveContractStateNotFound uuid)
        Just s  -> return s

invokeContractUpdate ::
    --    (MonadContract m, MonadError SCBError m)
    ( Member ContractEffect effs

    )
    => ActiveContractState
    -> Text
    -> JSON.Value
    -> Eff effs PartiallyDecodedResponse
invokeContractUpdate ActiveContractState { activeContract = ActiveContract {activeContractPath}
                                         , partiallyDecodedResponse
                                         } endpointName endpointPayload =
    invokeContract $
        UpdateContract activeContractPath $
        JSON.object
            [ ("oldState", newState partiallyDecodedResponse)
            , ( "event"
            , JSON.object
                    [("tag", JSON.String endpointName), ("value", endpointPayload)])
            ]

installedContractsProjection ::
       Projection (Set Contract) (StreamEvent key position ChainEvent)
installedContractsProjection = projectionMapMaybe contractPaths setProjection
  where
    contractPaths (StreamEvent _ _ (UserEvent (InstallContract contract))) =
        Just contract
    contractPaths _ = Nothing

activeContracts :: Member (EventLogEffect ChainEvent) effs => Eff effs (Set ActiveContract)
activeContracts = runGlobalQuery activeContractsProjection

activeContractsProjection ::
       Projection (Set ActiveContract) (StreamEvent key position ChainEvent)
activeContractsProjection = projectionMapMaybe contractPaths setProjection
  where
    contractPaths (StreamEvent _ _ (UserEvent (ContractStateTransition state))) =
        Just $ activeContract state
    contractPaths _ = Nothing

txHistory :: Member (EventLogEffect ChainEvent) effs => Eff effs [Ledger.Tx]
txHistory = runGlobalQuery txHistoryProjection

txHistoryProjection ::
       Projection [Ledger.Tx] (StreamEvent key position ChainEvent)
txHistoryProjection = projectionMapMaybe submittedTxs monoidProjection
  where
    submittedTxs (StreamEvent _ _ (NodeEvent (SubmittedTx tx))) = Just [tx]
    submittedTxs _                                              = Nothing

activeContractHistory ::
       Member (EventLogEffect ChainEvent) effs => UUID -> Eff effs [ActiveContractState]
activeContractHistory uuid = runGlobalQuery activeContractHistoryProjection
  where
    activeContractHistoryProjection ::
           Projection [ActiveContractState] (StreamEvent key position ChainEvent)
    activeContractHistoryProjection =
        projectionMapMaybe contractPaths monoidProjection
      where
        contractPaths (StreamEvent _ _ (UserEvent (ContractStateTransition state))) =
            if activeContractId (activeContract state) == uuid
                then Just [state]
                else Nothing
        contractPaths _ = Nothing

activeContractStates ::
       Member (EventLogEffect ChainEvent) effs => Eff effs (Map UUID ActiveContractState)
activeContractStates = runGlobalQuery activeContractStatesProjection
  where
    activeContractStatesProjection ::
           Projection (Map UUID ActiveContractState) (StreamEvent key position ChainEvent)
    activeContractStatesProjection =
        projectionMapMaybe contractStatePaths monoidProjection
      where
        contractStatePaths (StreamEvent _ _ (UserEvent (ContractStateTransition state))) =
            Just $ Map.singleton (activeContractId (activeContract state)) state
        contractStatePaths _ = Nothing

------------------------------------------------------------
-- | Create a database 'Connection' containing the connection pool
-- plus some configuration information.
dbConnect ::
    ( MonadUnliftIO m
    , MonadLogger m
    )
    => DbConfig
    -> m Connection
dbConnect DbConfig {dbConfigFile, dbConfigPoolSize} = do
    let connectionInfo = mkSqliteConnectionInfo dbConfigFile
    MonadLogger.logDebugN "Connecting to DB"
    connectionPool <- createSqlitePoolFromInfo connectionInfo dbConfigPoolSize
    pure $ Connection (defaultSqlEventStoreConfig, connectionPool)
