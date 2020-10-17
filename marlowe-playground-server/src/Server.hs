{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import           API                                              (API)
import qualified Auth
import           Auth.Types                                       (OAuthClientId (OAuthClientId),
                                                                   OAuthClientSecret (OAuthClientSecret))
import           Control.Monad.Except                             (ExceptT)
import           Control.Monad.IO.Class                           (MonadIO, liftIO)
import           Control.Monad.Logger                             (LoggingT, MonadLogger, logInfoN, runStderrLoggingT)
import           Control.Monad.Reader                             (ReaderT, runReaderT)
import           Data.Aeson                                       (ToJSON, eitherDecode, encode)
import           Data.Proxy                                       (Proxy (Proxy))
import           Data.Text                                        (Text)
import qualified Data.Text                                        as Text
import           Data.Validation                                  (Validation(..))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms (ContractTerms)
import           Language.Marlowe.ACTUS.Generator                 (genFsContract, genStaticContract)
import           Language.Marlowe.Pretty                          (pretty)
import           Network.Wai.Middleware.Cors                      (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import           Servant                                          ((:<|>) ((:<|>)), (:>), Application,
                                                                   Handler (Handler), Server, ServerError, hoistServer,
                                                                   serve, throwError)
import           Servant.Server.Internal.ServerError              (ServerError(..), err400)
import           System.Environment                               (lookupEnv)
import qualified Web.JWT                                          as JWT

genActusContract :: ContractTerms -> Handler String
genActusContract terms = pure . show . pretty . genFsContract $ terms

genActusContractStatic :: ContractTerms -> Handler String
genActusContractStatic terms =
    case genStaticContract terms of
        -- Failure errs -> throwError $ err400 { errBody = "Could not validate terms."} -- Figure out how to handle this
        Failure errs -> pure (unlines . (:) "ACTUS Term Validation Failed:" . map ((++) "    " . show) $ errs)
        Success c -> pure . show . pretty $ c

liftedAuthServer :: Auth.GithubEndpoints -> Auth.Config -> Server Auth.API
liftedAuthServer githubEndpoints config =
  hoistServer (Proxy @Auth.API) liftAuthToHandler Auth.server
  where
    liftAuthToHandler ::
      ReaderT (Auth.GithubEndpoints, Auth.Config) (LoggingT (ExceptT ServerError IO)) a ->
      Handler a
    liftAuthToHandler =
      Handler . runStderrLoggingT . flip runReaderT (githubEndpoints, config)

type Web = "api" :> (API :<|> Auth.API)

mkHandlers :: (MonadIO m) => AppConfig -> m (Server Web)
mkHandlers AppConfig {..} = do
  githubEndpoints <- liftIO Auth.mkGithubEndpoints
  pure $ (mhandlers :<|> liftedAuthServer githubEndpoints authConfig)

mhandlers :: Server API
mhandlers = genActusContract :<|> genActusContractStatic

app :: Server Web -> Application
app handlers =
  cors (const $ Just policy) . serve (Proxy @Web) $ handlers
  where
    policy =
      simpleCorsResourcePolicy

data AppConfig = AppConfig {authConfig :: Auth.Config}

initializeContext :: IO AppConfig
initializeContext = do
  putStrLn "Initializing Context"
  githubClientId <- getEnvOrEmpty "GITHUB_CLIENT_ID"
  githubClientSecret <- getEnvOrEmpty "GITHUB_CLIENT_SECRET"
  jwtSignature <- getEnvOrEmpty "JWT_SIGNATURE"
  redirectURL <- getEnvOrEmpty "GITHUB_REDIRECT_URL"
  let authConfig =
        Auth.Config
          { _configJWTSignature = JWT.hmacSecret jwtSignature,
            _configRedirectUrl = redirectURL,
            _configGithubClientId = OAuthClientId githubClientId,
            _configGithubClientSecret = OAuthClientSecret githubClientSecret
          }
  pure $ AppConfig authConfig

getEnvOrEmpty :: String -> IO Text
getEnvOrEmpty name = do
  mEnv <- lookupEnv name
  case mEnv of
    Just env -> pure $ Text.pack env
    Nothing -> do
      putStrLn $ "Warning: " <> name <> " not set"
      pure mempty

initializeApplication :: AppConfig -> IO Application
initializeApplication config = do
  handlers <- mkHandlers config
  pure $ app handlers
