module MainFrame
  ( mkMainFrame
  , handleAction
  , mkInitialState
  ) where

import View as View
import Data.Maybe (Maybe(..))
import Control.Monad.Reader (class MonadAsk, runReaderT)
import Control.Monad.State (class MonadState)
import Effect.Aff (Error)
import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, hoist)
import Halogen as H
import Control.Monad.Except.Trans (class MonadThrow)
import Halogen.HTML (HTML)
import Plutus.SCB.Webserver (SPParams_(SPParams_))
import Prelude
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Types (HAction(..), Query, State(..))

mkInitialState :: forall m. Monad m => m State
mkInitialState = do
  pure
    $ State
        {}

------------------------------------------------------------
ajaxSettings :: SPSettings_ SPParams_
ajaxSettings = defaultSettings $ SPParams_ { baseURL: "/api/" }

mkMainFrame ::
  forall m n.
  MonadThrow Error n =>
  MonadEffect n =>
  MonadAff m =>
  n (Component HTML Query HAction Void m)
mkMainFrame = do
  initialState <- mkInitialState
  pure $ hoist (flip runReaderT ajaxSettings)
    $ H.mkComponent
        { initialState: const initialState
        , render: View.render
        , eval:
          H.mkEval
            { handleAction: handleAction
            , handleQuery: const $ pure Nothing
            , initialize: Just Init
            , receive: const Nothing
            , finalize: Nothing
            }
        }

handleAction ::
  forall m.
  MonadState State m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  HAction -> m Unit
handleAction Init = pure unit
